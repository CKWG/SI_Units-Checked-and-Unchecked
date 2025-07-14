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
  -- Version   8.0
  -- Date      1 July 2025
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
  -- external_item ::= value [dimension]
  -- dimension     ::= * quotient | / divisor
  -- quotient      ::= product [ / divisor]
  -- divisor       ::= unit_factor | ( product )
  -- product       ::= unit_factor {* unit_factor}
  -- unit_factor   ::= [prefix] symbol [** exponent]
  -- exponent      ::= ( [sign] rational ) | numeral
  -- sign          ::= + | -
  -- rational      ::= numeral [ / numeral]
  -- numeral       ::= digit {digit}</pre>
  --
  -- There may be no space in an external item.
  --
  -- This is the syntax of an internal item:
  --
  -- internal_item ::= value * ""
  --                 | value * "quotient"
  --                 | value / "divisor"
  --
  -- There may be no space in the quotient nor the divisor.
  --
  -- prefix and symbol are as defined in SI (case sensitive);
  -- 'u' is used for micro instead of 'µ'; "Ohm" is used for the
  -- capital Omega.
  -- (There may be no spaces in the unit nor before of after the
  -- operator.)
  -- Additionally allowed symbols:
  --   min, h   for minute and hour      (no prefixes allowed)
  --   l, L     for liter                (   prefixes allowed)
  --
  -- Deviations from the syntax will propagate Illegal_Unit.
  --
  -- Examples: 1.0*"W/(m*K)"  correct
  --           1.0*"W/m/K"    wrong
  --           1.0 *"m"       wrong
  --
  -- The default representation of a unit is a product of all those
  -- unit factors (m, kg, s, A, K, cd, mol) which have an exponent
  -- different from 0, in that sequence.
  -- Each unit factor is given either as the pure symbol (if the
  -- exponent is 1), or as the symbol with integer or fractional
  -- exponent. The exponent is included in parentheses as necessary
  -- according to normal Ada rules.
  -- If the unit is 1, the default representation is empty.
  --
  -- In the non-default representation, unit factors may be given with
  -- any prefixes and symbols in any sequence and with any exponents.
  --
  -- Input from files or strings:
  -- Items may be read in any representation.
  -- If a nonzero value of Width is supplied, then exactly Width
  -- characters are input, or the characters (possibly none) up to a
  -- line terminator, whichever comes first; any skipped leading blanks
  -- are included in the count. [This sentence is an exact copy of
  -- RM A.10.9(19).]
  -- If the value of the parameter Width is zero, first the value is
  -- read as with Get of standard Text_IO.Float_IO (i.e. skipping any
  -- leading blanks, line terminators, or page terminators); Data_Error
  -- may be raised in this step. In case Data_Error was not raised,
  -- reading stops if no '*' or '/' follows (the item's unit being 1).
  -- Else characters are read as long as they belong to the set of
  -- constituent characters: letters 'A' .. 'Z' and 'a' .. 'z', digits,
  -- parentheses, operators, and signs. (This is similar to A.10.10(13)
  -- for enumeration literals.)
  -- The resulting sequence is evaluated according to the syntax.
  -- Illegal_Unit is raised if the sequence is not a valid representa-
  -- tion.
  --
  -- Output to files or strings:
  -- If no dimension format modifier is given, the default represen-
  -- tation is output.
  -- A dimension format modifier Dim must be a valid unit, optionally
  -- preceded by an operator (when omitted, implied as a '*'), and must
  -- be compatible with the item's dimension; else Illegal_Unit will be
  -- propagated.
  -- For output to strings, first the length of the unit is calculated,
  -- the remaining space will be used for the numeric part.
  --
  -- Examples:
  --
  --   S: Length := 1.0*"m";    default output  1.0*m
  --   V: Speed  := 1.0*"m/s";                  1.0*m*s**(-1)
  --   B: Item   := 1.0*"T";                    1.0*kg*s**(-2)*A**(-1)
  --
  -- The speed V above, when output with a Dim format modifier "km/s" or
  -- "*km/s", results in "1.0E-3*km/s".
  --
  -- The magnetic field B above may also be read as
  --   1.0E+3*mT
  --   1.0*m**(-2)*Wb
  --
  -- "1.0K" will be read a an item with value 1.0 and dimension 1,
  -- the following character 'K' will be left unconsumed (no '*' after
  -- the value). It will not be evaluated as one kelvin.
  -- "1.0*mB " will raise Illegal_Unit with the offending character 'B'
  -- (there is no unit symbol 'B'); the blank character will be left
  -- unconsumed. It will not be read as one meter with the following
  -- character 'B' left unconsumed.
  -- "1.0*mBqA " will raise Illegal_Unit with the offending character
  -- 'A'(there is no unit symbol 'BqA'); the blank character will be
  -- left unconsumed. It will not be read as one Millibecquerel with the
  -- following character 'A' left unconsumed.
  --
  -- Note: Precondition is commented out on Put procedures because they
  --       call Put to String.
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
                 Dim: in String := "") with Pre => Valid_Modifier (X, Dim) or else raise Unit_Error;

end Generic_SI.Generic_Text_IO;
