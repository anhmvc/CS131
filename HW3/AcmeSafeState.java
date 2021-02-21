import java.util.concurrent.atomic.AtomicLongArray;

class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() { 
        long[] longArr = new long[value.length()];
        for (int i = 0; i < value.length(); i++) {
            longArr[i] = value.get(i);
        }
        return longArr; 
    }

    public void swap(int i, int j) {
	value.getAndDecrement(i); // value[i]--;
	value.getAndIncrement(i); // value[j]++;
    }
}