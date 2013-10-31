import gnu.trove.map.hash.TCustomHashMap;
import gnu.trove.strategy.IdentityHashingStrategy;

public class TStringStringHashMapFactory {
  public static TCustomHashMap<String, String> create() {
    return new TCustomHashMap<String, String>(new IdentityHashingStrategy<String>());
  }
}
