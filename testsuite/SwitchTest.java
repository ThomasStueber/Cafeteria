
public class SwitchTest {
    public SwitchTest() {}

    public void testSwitch() {
        int i = 5;
        switch (i) {
            case 0:
                i++;
                break;
            case 1:
                i++;
                break;
            case 2:
                i += 2;
                break;
            case 3:
                i--;
                break;
            case 4:
                i *= 2;
                break;
            case 5:
                i /= 2;
                break;
            default:
                i = -1;
                break;
        }
    }
}
