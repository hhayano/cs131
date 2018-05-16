import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
  private AtomicIntegerArray value;
  private byte maxval;

  GetNSetState(byte[] v) {
    int[] intv = new int[v.length];

    for (int i = 0; i < v.length; i++) 
      intv[i] = (int) v[i];

    value = new AtomicIntegerArray(intv);
    maxval = 127;
  }

  GetNSetState(byte[] v, byte m) {
    int[] intv = new int[v.length];

    for (int i = 0; i < v.length; i++) 
      intv[i] = (int) v[i];

    value = new AtomicIntegerArray(intv);
    maxval = m;
  }

  public int size() { return value.length(); }

  public byte[] current() {
    int arrayLength = value.length();
    byte[] v = new byte[arrayLength];
    
    for (int i = 0; i < arrayLength; i++) {
      v[i] = (byte) value.get(i);
    }

    return v;
  }

  public boolean swap(int i, int j) {
    if (value.get(i) <= 0 || value.get(j) >= maxval)
      return false;

    value.getAndDecrement(i);
    value.getAndIncrement(j);
    return true;
  }
}
