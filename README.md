# SI_Units Checked and Unchecked
Ada packages for handling SI units of measure:
  V: Speed  := 5.0*"km/h";  -- unit indication is
  T: Time   := 30.0*"s";    -- case sensitive: "S" is Siemens
  D: Length := V * T;       -- dimension "m"
  Put (V, Dim => "dam");    -- output with any prefix, here decameter

The method comes in two variants, checked and unchecked, selected with a generic parameter:

Checked: Physical items carry with them their current dimension, so that all assignments and expressions are checked with respect to correct dimensions.

Unchecked: This variant has the exact same specification, but physical items only carry their numerical values, dimensions are stripped.

This is release 2.0.0.
Incompatible User Interface change: 'Image attribute redefined.
Some improvements of documentation and tests.
Tested with GNAT CE 2021.
Use of GNAT 15.2.1 is not recommended because of Bugzilla 122574.

License GPL 3 with GNAT modification.

For more information, see file SI.html.
