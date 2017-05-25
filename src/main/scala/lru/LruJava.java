package lru;

import java.util.HashMap;
import java.util.Map;

public class LruJava<K,V> {

    public LruJava(int size) {
        capacity = size;
    }

    private int capacity;

    private Map<K, Node> map = new HashMap<>();

    class Node {
        Node prev;
        Node next;
        K key;
        V value;

        Node(Node prev, Node next, K k, V v) {
            this.prev = prev;
            this.next = next;
            key = k;
            value = v;
        }
    }

    private Node head, tail;


    public V get(K k) {
        Node nResult = map.get(k);
        if(nResult == null) return null;
        if(nResult.equals(tail))
            tail = tail.next;
        remove(nResult.key);
        set(nResult.key, nResult.value);
        return nResult.value;
    }

    public void set(K k, V value) {
        // is the k already in the cache?
        //   yes: remove key k
        // is the cache already full?
        //   yes: remove least recently used key
        //     update tail
        //     remove old tail
        // add k,v

        Node e = map.get(k);
        if(e != null) remove(k);
        if(map.size() >= capacity) {
            K tailKey = tail.key;
            tail = tail.next;
            remove(tailKey);
        }

        Node newNode = new Node(head, null, k, value);
        if (head != null)
            head.next = newNode;
        if(tail == null)
            tail = newNode;
        head = newNode;
        map.put(k, newNode);

    }

    private void remove(K k) {
        Node n = map.get(k);
        if(n.prev != null)
            n.prev.next = n.next;
        if(n.next != null)
            n.next.prev = n.prev;
        map.remove(n.key);
    }


    public static void main(String[] args) {

        LruJava<Character, Character> lru = new LruJava<>(3);
        lru.set('a', 'A');
        lru.set('b', 'B');
        lru.set('c', 'C');
        lru.set('d', 'D');
        System.out.println(lru.get('a'));
        System.out.println(lru.get('b'));
        lru.set('e', 'E');
        System.out.println(lru.get('c'));
        System.out.println(lru.get('d'));
        System.out.println(lru.get('b'));
        System.out.println(lru.get('e'));

    }

}

