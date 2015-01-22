public class Test {
    /* multi line
     * comment test */
    private int x;
    // single line comment
    private int y;

    /* Constructor */
    public Test() {
        x = 0;
        y = 0;
    }

    public void assign(int _x, int _y) {
        x = _x;
        y = _y;
    }

    public int calculate(int _x, int _y) {
        x++;
        y--;
        x *= _x;
        y /= _y;
        x += _y;
        y -= _x;
        return _x;
    }


    public void assignBool(boolean a, boolean b) {
        a = true;
        b = false;
    }

    public boolean xor(boolean a, boolean b) {
        return a != b;
    }

    public void conditionalTest() {
        if (x > 1) {
            x++;
        } else  if (x < 1) {
            x--;
        } else {
            x = 0;
        }
    }

    public void forLooptest() {
        int j;
        if (x >=0)
            j = x;
        else
            j = -x;
        for (int i = 0; i < j; i++) {
            y += (i + j) / 2;
        }
    }

    public void whileLoopTest() {
        int j;
        if (x >=0)
            j = x;
        else
            j = -x;
        int i = 0;
        while (i < j) {
            y += (i + j) / 2;
            i++;
        }
    }

    public void unreachableCodeTest() {
        int i = 2;
        // return; // uncomment to test error message
        i = 4;
    }

    public void switchTest() {
        switch (x) {
            case 0:
                x++;
                break;
            case 1:
                x--;
                break;
            default:
                x = -1;
                break;
        }
    }

    public void testNumberFormats() {
        int binary = 0b01011;
        int octal = 012;
        int hex = 0x14;
        // double d = 3.4d;
        // float f = 4.1f;
    }
}
