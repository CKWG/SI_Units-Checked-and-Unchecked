# SI_Units Checked and Unchecked
Ada packages for handling SI units of measure:

Physical items have a private type; a subtype is defined for any dimension that has a unique SI unit like Newton and a lot more like speed.
Units (meter `"m"`, second `"s"`, Newton `"N"`, etc. together wirh optional prefixes) are given as strings because they are case sensitive.

`  V: Speed  := 5.0*"km/h";  -- unit indication is ...`<br>
`  T: Time   := 30.0*"s";    -- ... case sensitive: "S" is Siemens`<br>
`  D: Length := V * T;       -- dimension "m"`<br>
`  Put (D, Dim => "dam");    -- output with any prefix, here decameter: 4.2*dam (no string quotes)`<br>
`  Put (D, Dim => "Km");     -- raises Illegal_Unit, wrong case 'K'`<br>
`  Put (V, Dim => "dam");    -- raises Unit_Error, not dimension Speed`

The method comes in two variants, checked and unchecked, selected with a generic parameter:

- *Checked*: Physical items carry with them their current dimension, so that all assignments and expressions are checked with respect to correct dimensions.
- *Unchecked*: This variant has the exact same specification, but physical items only carry their numerical values, dimensions are stripped.

License GPL 3 with GNAT modification.

For a more detailed introduction, see file SI.html.
There is a complete user interface documentation included in a separate directory about which units are supported, what is the unit string syntax etc.

[Also see the author's [Ada Magica](https://www.ada-deutschland.de/sites/default/files/AdaTourCD/AdaTourCD2004/Ada%20Magica/Contents.html) and [Ada introductory course](https://www.ada-deutschland.de/sites/default/files/AdaKursGrein/Ada-Kurs-Course.html) at [Ada Deutschland](https://www.ada-deutschland.de/).]

**This is an interim update; May 23, 2026**<br>
1. The previous `SI.Text_IO.Get` operation with parameter `Width/= 0` was not very plausible.
   `Ada.Text_IO.Get` with `Width/= 0` is most probably meant for reading data tables,
   so data to be read is right-aligned and trailing characters within `Width` range lead to `Data_Error`.<br>
   `        1815`<br>
   `        1983`<br>
   `        1995`<br>
Tables with dimensioned data generally split the numeric value and the unit in separate columns, so they cannot sensibly be read without additional parameters.<br>
   `        15.0   km`<br>
   `          60   km/h`<br>
This change does not affect legacy code since the new parameters have default values.
Only calls of `Get` with `Width/=0` will no longer work, but those will be rare if not nonexistent (because of unusability).
Of course the corresponding Put operation has also new defaulted parameters.
3. Plugged a hole in type checking:<br>
`Put (X, Dim => "");    -- outputs the default unit, no checks`<br>
`Put (X, Dim => "mm");  -- checks whether X has dimension Length`<br>
`Put (X, Dim => " ");   -- new, note the space: checks whether X has dimension One`
4. Tests have been updated. The Tables example shows the new functionality.

**This is release 2.0.0; Apr 7, 2026.**<br>
Incompatible User Interface change: Ada 2022 defines the 'Image attribute for any type and allows its redefinition &ndash;
since its default is impractical, the 'Image attribute has been redefined to deliver the same result as its previous homonymous
replacement function, which does no longer exist; its partner, the Value function, has been moved to the base package where
'Image is now defined.<br>
Some improvements of documentation and tests.<br>
Tested with GNAT CE 2021.<br>
Use of GNAT 15.2.1 is not recommended because of Bugzilla 122574.

**Release 1.0.1; Sep 9, 2025**<br>
Evaluation of unit strings has been reimplemented, tests adapted accordingly (no effect on execution time).
There is no change in the user interface.
Only behaviour in case of wrong unit strings is different.
There is a bug fix in the unit string syntax.<br>
Tested with GNAT CE 2021.

**Release 1.0.0; Jul 14, 2025**<br>
First GitHub release.<br>
Updated edition of 5 Aug 2020 published at http://archive.adaic.com/tools/CKWG/Dimension/SI.html.
The source at this URL is no longer updated.<br>
Uses Predicates and (new) pragma Assertion_Policy.<br>
Code: Celsius temperature scale added; instantiation of Text_IO package changed.<br>
Test: Improved IO tests.<br>
Since a physical item is of a private type, the attributes 'Image and 'Value do not exist.
Two homonymous functions are used as a replacement.
