
// This test should fail because we don't cast the integer to a character
public class MissingCastTest {
    {
        int i = 5;
        char c = i;
    }
}
