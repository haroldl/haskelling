Haskell has a beautiful pattern where you can zip a lazy list of
values with itself. You start with an initialValue. The next value is
the result of applying the stepFunction to the initialValue and the
first input. In Java style pseudo code, you're building an array like so:

    values[0] = inputs[0];
    for (int i = 1; i < inputs.length; i++)
    {
      values[i] = stepFunction(values[i-1], inputs[i]);
    }

In Haskell this becomes:

    values = initialValue : zipWith stepFunction inputs values

These are both O(n) time. In C/Java this takes up O(n) space for the
extra array of values. But in Haskell it can be O(1) space if you're
just scanning the values one at a time.
