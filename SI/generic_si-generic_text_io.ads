------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2008, 2018, 2020, 2025
--               Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

generic

  with package Real_Text_IO is new Float_IO (Real'Base);

package Generic_SI.Generic_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   8.3
  -- Date      15 September 2025
  --====================================================================
  -- For the numeric part, Put and Get work like the conventional
  -- operations in Ada.Text_IO, the formatting parameters apply to
  -- the numeric value only, not to the dimension.
  --
  -- An item is a value followed by its unit. There are two forms of
  -- items: an internal one as written in programs (e.g. 1.0*"m")
  -- and an external one as output to or read from files or strings
  -- (e.g. 1.0*m).
  --
  -- This is the syntax of an external item:
  --
  --    external_item ::= value [dimension]
  --    dimension     ::= * quotient | / divisor
  --    quotient      ::= product [ / divisor]
  --    divisor       ::= unit_factor | ( product )
  --    product       ::= unit_factor {* unit_factor}
  --    unit_factor   ::= identifier [** exponent]
  --    identifier    ::= si_letter {si_letter}
  --    exponent      ::= ( [sign] rational ) | numeral
  --    sign          ::= + | -
  --    rational      ::= numeral [ / numeral]
  --    numeral       ::= digit {digit}</pre>
  --
  -- There may be no space in an external item.
  --
  -- This is the syntax of an internal item:
  --
  --    internal_item   ::= value * quotient_string
  --                      | value / product_string
  --    quotient_string ::= ""
  --                      | "quotient"
  --    product_string  ::= "product"
  --
  -- There may be no space between the string quotes.
  --
  -- An si_letter is a character that is in one of the ranges 'A'..'Z'
  -- or 'a'..'z' or is 'µ'. The identifier must be interpretable as
  -- [prefix] symbol.
  --
  -- prefix and symbol are as defined in SI (case sensitive);
  -- 'u' may be used for micro instead of 'µ'; "Ohm" is used for the
  -- capital Omega.
  --
  -- Any prefixed SI unit may be used as defined in The International
  -- System of Units (SI) published by the Bureau International des
  -- Poids et Mesures. Of the non-SI units listed in this document,
  -- only hectare (ha), minute (min), hour (h), liter (l, L), and
  -- electronvolt (eV) are currently allowed; of these, only the last
  -- three may take prefixes.
  --
  -- Note: "ha" (100*(10*m)**2 = (100*m)**2 = 10_000*m**2) is a special
  -- case; it is composed of the prefix "hecto" and the unit "are"
  -- ((10*m)**2 = 100*m**2), but "are" is not allowed with SI, thus no
  -- prefixes are allowed and neither e.g. "ka" nor the alleged 1*km**2
  -- "hha" are legal units.
  --
  -- The complete list of symbols can be found in package body
  -- Generic_SI.Generic_Symbols.
  --
  -- Deviations from the syntax will propagate Illegal_Unit.
  --
  -- In the Checked variant, the default representation of the dimension
  -- is a product of all base units (m, kg, s, A, K, cd, mol in that
  -- sequence) with an exponent different from 0. If the dimension is 1,
  -- the default representation is empty. Each base unit is given either
  -- as the pure unit (if the exponent is 1), or as the unit with an
  -- integer exponent if the exponent is a whole number (other than 1),
  -- or as the unit with a fractional exponent. The exponent is included
  -- in parentheses as necessary according to normal Ada rules.

  -- In the Unchecked variant, the default representation of the
  -- dimension is empty.
  --
  -- In the non-default representation, unit factors may be given with
  -- any prefixes and symbols in any sequence and with any exponents.
  --
  -- Input and Output
  --
  -- This is the syntax of a dimension format modifier (there may be no
  -- spaces between the string quotes):
  --
  --    modifier ::= "" | "[*]quotient" | "/divisor"
  --
  -- A dimension format modifier must be a valid representations of the
  -- dimension.
  --
  -- Input from files or strings:
  --
  -- Items may be read in any representation.
  --
  -- If the value of the parameter Width is zero, first the value is
  -- read as with Get of standard Text_IO.Float_IO (i.e. skipping any
  -- leading blanks, line terminators, or page terminators;
  -- see RM A.10.9(13)); Data_Error may be raised in this step. In case
  -- Data_Error is not raised, then reads the longest possible sequence
  -- of characters matching the syntax given above. [This sentence is an
  -- exact copy of RM A.10.9(13), applied to the unit string.]
  --
  -- If a nonzero value of Width is supplied, then exactly Width
  -- characters are input, or the characters (possibly none) up to a
  -- line terminator, whichever comes first; any skipped leading blanks
  -- are included in the count. [This sentence is an exact copy of
  -- RM A.10.9(19).]
  --
  -- Returns in the parameter X the value of type Item that corresponds
  -- to the sequence input. Illegal_Unit is raised if the sequence input
  -- does not have the required syntax. [This sentence is an adaptation
  -- of RM A.10.9(21).]
  --
  -- Output to files or strings:
  --
  -- If the dimension format modifier is "", the numeric value and the
  -- default representation are output. If a modifier is given, the
  -- numeric value modified according to the prefixes is output followed
  -- by the modifier. The precondition has been commented out because
  -- otherwise the modifier would be evaluated at least twice.
  --
  -- For output to files, the numeric part is output first, followed by
  -- the dimension. For output to strings, first the length of the
  -- dimension string is calculated, the remaining string part is used
  -- for the numeric value.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.07.2002
  --  C.G.    1.1  03.08.2002 Allow '/' when reading dimension
  --  C.G.    1.2  06.08.2002 Dim format modifier for Put added
  --  C.G.    2.0  24.01.2003 Syntax for reading unit symbols changed
  --  C.G.    3.0  28.04.2008 Added Width to Get and IO with strings
  --  C.G.    4.0  29.07.2008 Additional_Units
  --  C.G.    5.0  28.07.2018 Unit strings (Additional_Units removed)
  --  C.G.    5.1  28.09.2018 Unit syntax changed W/m/K => W/(m*K)
  --  C.G.    6.0  13.05.2020 Parent renamed to Generic_SI
  --  C.G.    7.0  18.04.2025 Precondition replaces check in body
  --  C.G.    8.0  01.07.2025 generic param. as in Text_IO for Celsius
  --  C.G.    8.1  12.08.2025 Improved description
  --  C.G.    8.2  29.08.2025 Preconditions commented out
  --  C.G.    8.3  15.09.2025 Allow µ; corrected typos in description
  --====================================================================

  pragma Elaborate_Body;

  function Valid_Modifier (X: Item; Dim: String) return Boolean;

  Default_Fore: Field := 2;
  Default_Aft : Field := Real'Digits - 1;
  Default_Exp : Field := 3;

  procedure Get (X    : out Item;
                 Width: in  Field := 0);
  procedure Get (File : in  File_Type;
                 X    : out Item;
                 Width: in  Field := 0);

  procedure Put (X   : in Item;
                 Fore: in Field  := Default_Fore;
                 Aft : in Field  := Default_Aft;
                 Exp : in Field  := Default_Exp;
                 Dim : in String := "");  -- with Pre => Valid_Modifier (X, Dim) or else raise Unit_Error
  procedure Put (File: in File_Type;
                 X   : in Item;
                 Fore: in Field  := Default_Fore;
                 Aft : in Field  := Default_Aft;
                 Exp : in Field  := Default_Exp;
                 Dim : in String := "");  -- with Pre => Valid_Modifier (X, Dim) or else raise Unit_Error

  procedure Get (From: in  String;
                 X   : out Item;
                 Last: out Positive);
  procedure Put (To : out String;
                 X  : in Item;
                 Aft: in Field  := Default_Aft;
                 Exp: in Field  := Default_Exp;
                 Dim: in String := "");  -- with Pre => Valid_Modifier (X, Dim) or else raise Unit_Error

end Generic_SI.Generic_Text_IO;
