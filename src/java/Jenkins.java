public class Jenkins {
  public static int hash(byte[] key) {
    int hash = 0;
    for (byte b : key) {
      hash += (b & 0xFF);
      hash += (hash << 10);
      hash ^= (hash >>> 6);
    }
    hash += (hash << 3);
    hash ^= (hash >>> 11);
    hash += (hash << 15);
    return hash;
  }
  public static int hashString(String s) {
    return hash(s.getBytes());
  }
  public static int hashPair(int i, int j) {
    int hash = 23;
    hash = hash * 31 + i;
    hash = hash * 31 + j;
    return hash;
  }
  public static int hashTuple(int i, int j, int k) {
    int hash = 23;
    hash = hash * 31 + i;
    hash = hash * 31 + j;
    hash = hash * 31 + k;
    return hash;
  }
  public static int hashQuadruplet(int i, int j, int k, int l) {
    int hash = 23;
    hash = hash * 31 + i;
    hash = hash * 31 + j;
    hash = hash * 31 + k;
    hash = hash * 31 + l;
    return hash;
  }
}
