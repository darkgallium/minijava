class Dumb {
    public static void main(String[] args) {
	     System.out.println(new A().a());
    }
}
class A {
  int a;
  public int a() {
    System.out.println(42);
    return 0;
  }
}
