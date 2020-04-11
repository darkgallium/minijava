class Main {
    public static void main(String[] args) {
        System.out.println(new Shape().init());
    }
}

class Shape {
    public int init() {
        Shape s;
        Rectangle r;
        Circle c;
        int tmp;

        s = new Shape();
        r = new Rectangle();
        c = new Circle();

        tmp = s.draw();
        tmp = r.draw();
        tmp = c.draw();

        s = r;
        tmp = s.draw();

        s = c;
        tmp = s.draw();
        return 0;
    }
    public int draw() {
        System.out.println(100);
        return 0;
    }
}

class Rectangle extends Shape {
    public int draw() {
        System.out.println(101);
        return 0;
    }
}

class Circle extends Shape {
    public int draw() {
        System.out.println(102);
        return 0;
    }
}
