import java.util.concurrent.locks.ReentrantLock;

class BetterSafe implements State {
  private byte[] value;
  private ReentrantLock[] locks;
  private byte maxval;

  BetterSafe(byte[] v) {
    value = v;
    maxval = 127;

    locks = new ReentrantLock[v.length];
    for (int i = 0; i < v.length; i++)
      locks[i] = new ReentrantLock();
  }

  BetterSafe(byte[] v, byte m) { 
    value = v;
    maxval = m;

    locks = new ReentrantLock[v.length];
    for (int i = 0; i < v.length; i++)
      locks[i] = new ReentrantLock();
  }

  public int size() { return value.length; }

  public byte[] current() { return value; }

  public boolean swap(int i, int j) {
    if (i < j) {
      locks[i].lock();
      locks[j].lock();
    } else {
      locks[j].lock();
      locks[i].lock();
    }

    if (value[i] <= 0 || value[j] >= maxval) {
      locks[i].unlock();
      locks[j].unlock();
      return false;
    }

    value[i]--;
    value[j]++;
    locks[i].unlock();
    locks[j].unlock();
    return true;
  }
}
