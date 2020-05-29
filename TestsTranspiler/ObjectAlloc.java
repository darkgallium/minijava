class Dumb {
    public static void main(String[] args) {
      A obj;
      obj = new A();
      System.out.println(obj.a());
    }
}
class A {
  int a;
  public int a() {
    System.out.println(42);
    return 0;
  }
}
