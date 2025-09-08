# SI_Units Checked and Unchecked
Ada packages for handling SI units of measure

The method comes in two variants, checked and unchecked, selected with a generic parameter:

Checked: Physical items carry with them their current dimension, so that all assignments and expressions are checked with respect to correct dimensions.

Unchecked: This variant has the exact same specification, but physical items only carry their numerical values, dimensions are stripped.

This is release 1.0.1.
Evaluation of unit strings has been reimplemented, tests adapted accordingly (no effect on execution time). There is no change in the user interface. Only behaviour in case of wrong unit strings is different.
There is a bug fix in the unit string syntax.
Tested with GNAT CE 2021.

For more information, see file SI.html.
