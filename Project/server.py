import re
import sys
import time
import json
import asyncio
import aiohttp

PORTS = {
    "Riley": 15440, 
    "Jaquez": 15441, 
    "Juzang": 15442, 
    "Campbell": 15443, 
    "Bernard": 15444,
}

NEIGHBORS = {
    "Riley": ["Jaquez", "Juzang"],
    "Jaquez": ["Riley", "Bernard"],
    "Juzang": ["Riley", "Campbell", "Bernard"],
    "Campbell": ["Juzang", "Bernard"],
    "Bernard": ["Juzang", "Jaquez", "Campbell"],
}

API_KEY = "GOOGLE_API_KEY"

if len(sys.argv) != 2:
    sys.exit("ERROR: Invalid arguments. Correct usage: python3 server.py [SERVER_ID]")

if sys.argv[1] not in PORTS:
    sys.exit("ERROR: Invalid server ID. Acceptable IDs: Riley, Jaquez, Juzang, Campbell, Bernard")

serverID = sys.argv[1]
PORT = PORTS[serverID]
CLIENTS = {}
# redirect stdout outputs to a log file
sys.stdout = open(f'{serverID}Log.txt', 'w') 

def convertCoords(coords):
    for i in range(len(coords)):
        char = coords[i]
        if i != 0 and char == "+" or char == '-':
            break
    X = coords[:i]
    Y = coords[i:]
    return f"{X},{Y}"

def getTimeDiff(start, end):
    diff = float(end) - float(start)
    if diff > 0:
        return "+%.9f" % diff
    else:
        return "%.9f" % diff

def isPatternMatched(pattern, string):
    return bool(re.match(pattern, string))

async def sentToClient(response, reader, writer):
    print(f"Sent to Client: {response}", end='')
    writer.write(response.encode())
    await writer.drain()

    writer.close()
    await writer.wait_closed()

async def handle_connections(reader, writer):
    validCommands = ["IAMAT", "WHATSAT", "AT"]
    senderName = "Client"
    isValid = False

    data = await reader.read()
    serverTime = "%.9f" % time.time() 
    message = data.decode()
    command = message.split()
    NAME = command[0]

    if NAME == "AT" and len(command) == 8 and command[-2] in PORTS and command[-1].isnumeric():
        senderName = command[-2]
        isValid = True
 
    print(f"Received from {senderName}: {message}", end='')

    # invalid commands
    if NAME not in validCommands \
        or ((NAME == "IAMAT" or NAME == "WHATSAT") and len(command) != 4) \
        or (NAME == "IAMAT" and (not isPatternMatched("[+-][0-9]{1,3}[.][0-9]+[+-][0-9]{1,3}[.][0-9]+",command[2]) or not isPatternMatched("[0-9]{9,}[.][0-9]{9}", command[3]))) \
        or (NAME == "WHATSAT" and (int(command[2]) < 0 or int(command[2]) > 50 or int(command[3]) < 0 or int(command[3]) > 20)) \
        or (NAME == "AT" and len(command) != 8 and not isValid):
        response = f"? {message}"
        await sentToClient(response, reader, writer)

    elif NAME == "IAMAT":
        clientID = command[1]
        if clientID not in CLIENTS:
            CLIENTS[clientID] = {}
        coords = CLIENTS[clientID]["coords"] = command[2]
        clientTime = CLIENTS[clientID]["time"] = command[3]
        timeDiff = CLIENTS[clientID]["diff"] = getTimeDiff(clientTime, serverTime)
        
        response = f"AT {serverID} {timeDiff} {clientID} {coords} {clientTime}\n"

        await sentToClient(response, reader, writer)

        # propogate responses throughout the network
        for receiver in NEIGHBORS[serverID]:
            receiverPort = PORTS[receiver]
            try:
                fwdReader1, fwdWriter1 = await asyncio.open_connection("127.0.0.1", receiverPort)
            
                fwdMessage = f"AT {serverID} {timeDiff} {clientID} {coords} {clientTime} {serverID} 2\n"

                print(f'Sent to {receiver}: {fwdMessage}', end='')
                fwdWriter1.write(fwdMessage.encode())
                await fwdWriter1.drain()
            
                fwdWriter1.close()
                await fwdWriter1.wait_closed()
            except ConnectionError:
                print(f"ERROR: Unable to connect to {receiver}")  

    elif NAME == "WHATSAT":
        clientID = command[1]
        if clientID not in CLIENTS:
            response = "ERROR: No info available about this client\n"
            await sentToClient(response, reader, writer)

        else:
            radius = command[2]
            limit = int(command[3])
            coords = convertCoords(CLIENTS[clientID]['coords'])

            async with aiohttp.ClientSession() as session:
                async with session.get(f"https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={API_KEY}&location={coords}&radius={radius}") as resp:
                    googleResp = await resp.json()
                    googleResp["results"] = googleResp["results"][:limit]
                    googleResp = json.dumps(googleResp, indent=4)

            clientInfo = f"AT {CLIENTS[clientID]['server']} {CLIENTS[clientID]['diff']} {clientID} {CLIENTS[clientID]['coords']} {CLIENTS[clientID]['time']}\n"
            response = clientInfo + googleResp + "\n\n"
            await sentToClient(response, reader, writer)

    elif NAME == "AT" and senderName != "Client":
        # update values
        clientID = command[3]
        clientTime = command[5]
        if clientID not in CLIENTS:
            CLIENTS[clientID] = {}
        clientInfo = CLIENTS[clientID]
        lastServer = command[1]
        timeDiff = command[2]
        coords = command[4]
        clientTime = command[5]
        count = int(command[7])
        
        # only update if clientTime is after time stored
        if 'time' not in clientInfo or (float(clientTime) > float(clientInfo['time'])):
            clientInfo['server'] = lastServer
            clientInfo['diff'] = timeDiff
            clientInfo['coords'] = coords
            clientInfo['time'] = clientTime

        if count > 0:
            for receiver in NEIGHBORS[serverID]:
                receiverPort = PORTS[receiver]
                try:
                    print(f"NEW CONNECTION TO {receiver}")
                    fwdReader2, fwdWriter2 = await asyncio.open_connection("127.0.0.1", receiverPort)
                
                    fwdMessage = f"AT {lastServer} {timeDiff} {clientID} {coords} {clientTime} {serverID} {count-1}\n"

                    print(f'Sent to {receiver}: {fwdMessage}', end='')
                    fwdWriter2.write(fwdMessage.encode())
                    await fwdWriter2.drain()
                
                    fwdWriter2.close()
                    await fwdWriter2.wait_closed()
                except ConnectionError:
                    print(f"ERROR: Unable to connect to {receiver}")

async def main():
    server = await asyncio.start_server(
        handle_connections, '127.0.0.1', PORT)

    addr = server.sockets[0].getsockname()
    print(f'Serving {serverID} on {addr!r}...')

    async with server:
        await server.serve_forever()
    
    sys.stdout.close()

asyncio.run(main())
