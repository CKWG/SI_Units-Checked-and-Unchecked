------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2006, 2011, 2018, 2020, 2025
-- Christoph Karl Walter Grein
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

with Generic_SI.Generic_Unformatted_IO,
     Generic_SI.Generic_Symbols;

package body Generic_SI is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   8.0
  -- Date      15 October 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  05.07.2002 with Gnat 3.14p work-around
  --  C.G.    2.0  15.09.2002 Of course the argument of trigonometric
  --                          functions with cycle can be dimensioned
  --  C.G.    2.1  27.09.2002 Gnat work-around removed for 3.16w
  --  C.G.    2.2  15.11.2002 Use Real'Base
  --  C.G.    2.3  21.02.2003 Add Dimension_as
  --  C.G.    2.4  20.03.2006 Generic_Elementary_Functions to spec
  --  C.G.    3.0  13.12.2011 New subpackage for_Test_only
  --  C.G.    4.0  01.08.2018 Unit strings
  --  C.G.    4.1  10.08.2018 Bug fix in inverse trigonometric functions
  --  C.G.    4.2  31.08.2018 Dimensionless ""
  --  C.G.    4.3  28.09.2018 Value returns Real'Base
  --  C.G.    5.0  13.05.2020 Renamed Generic_SI from Checked_SI;
  --                          removed child package Generic_Strings
  --  C.G.    5.1  05.08.2020 Bug fix in has_Dimension
  --  C.G.    6.0  14.04.2025 Preconditions replace check in body
  --  C.G.    6.1  31.07.2025 Bug fix in "/" (Left: Real; Right: String)
  --  C.G.    7.0  03.08.2025 New implementation: Use FSM
  --  C.G.    7.1  08.08.2025 Added exception message in function
  --                          "*" (Left: Real'Base; Right: String)
  --  C.G.    7.2  13.08.2025 Make Construct visible in private part
  --  C.G.    7.3  16.08.2025 Implementation has_Dimension changed
  --  C.G.    7.4  22.08.2025 Common interface for evaluating all kinds
  --                          of unit indications
  --  C.G.    8.0  15.10.2025 Ada 2022: Redefine 'Image attribute;
  --                          Generic_Strings renamed to
  --                          Generic_Unformatted_IO
  --====================================================================

  package Unformatted_IO is new Generic_Unformatted_IO;

  function Value (X: String) return Item renames Unformatted_IO.Value;

  procedure Image (B: in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class; X: Item) is
  begin
    B.Put (Unformatted_IO.Image (X));
  end Image;

  function Same_Dimension (X, Y: Item) return Boolean is (X.Unit = Y.Unit);

  function has_Dimension (X: Item; Symbol: String) return Boolean is
  begin
    if Symbol = "" then
      return Same_Dimension (X, One);
    else
      declare
        S: constant String := (if Symbol (1) in '*' | '/' then "rad" else "") & Symbol;
      begin
        return Same_Dimension (X, 1.0 * S);
      end;
    end if;
  end has_Dimension;

  function Value (X: Item) return Real'Base is (X.Value);

  -- Real arithmetic

  function to_Real (X: Rational) return Real'Base is
    pragma Inline (to_Real);
  begin
    return Real'Base (Numerator (X)) / Real'Base (Denominator (X));
  end to_Real;

  function "**" (Base: Real'Base; Exponent: Whole) return Real'Base is
    pragma Inline ("**");
  begin
    return Base ** Integer (Exponent);
  end "**";

  function "**" (Base: Real'Base; Exponent: Rational) return Real'Base is
    pragma Inline ("**");
  begin
    return Base ** to_Real (Exponent);
  end "**";

  -- Operators

  function "abs" (Right: Item) return Item is
  begin
    return (Right.Unit, abs Right.Value);
  end "abs";

  function "+" (Right: Item) return Item is
  begin
    return Right;
  end "+";

  function "-" (Right: Item) return Item is
  begin
    return (Right.Unit, -Right.Value);
  end "-";

  function "+" (Left, Right: Item) return Item is
  begin
    return (Left.Unit, Left.Value + Right.Value);
  end "+";

  function "-" (Left, Right: Item) return Item is
  begin
    return (Left.Unit, Left.Value - Right.Value);
  end "-";

  function "*" (Left, Right: Item) return Item is
  begin
    return (Left.Unit  * Right.Unit ,
            Left.Value * Right.Value);
  end "*";

  function "/" (Left, Right: Item) return Item is
  begin
    return (Left.Unit  / Right.Unit ,
            Left.Value / Right.Value);
  end "/";

  function "*" (Left: Item; Right: Real'Base) return Item is
  begin
    return (Left.Unit, Left.Value * Right);
  end "*";

  function "*" (Left: Real'Base; Right: Item) return Item is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Item ; Right: Real'Base) return Item is
  begin
    return (Left.Unit, Left.Value / Right);
  end "/";

  function "/" (Left: Real'Base; Right: Item) return Item is
  begin
    return (uno  / Right.Unit,
            Left / Right.Value);
  end "/";

  function "**" (Base: Item; Exponent: Whole) return Item is
  begin
    return (Base.Unit  ** Exponent,
            Base.Value ** Exponent);
  end "**";

  function "**" (Base: Item; Exponent: Rational) return Item is
  begin
    return (Base.Unit  ** Exponent,
            Base.Value ** Exponent);
  end "**";

  function "**" (Base: Dimensionless; Exponent: Real'Base) return Dimensionless is
  begin
    return (uno,
            Base.Value ** Exponent);
  end "**";

  function "<" (Left, Right: Item) return Boolean is
  begin
    return Left.Value < Right.Value;
  end "<";

  function "<=" (Left, Right: Item) return Boolean is
  begin
    return Left.Value <= Right.Value;
  end "<=";

  function ">=" (Left, Right: Item) return Boolean is
  begin
    return Left.Value >= Right.Value;
  end ">=";

  function ">" (Left, Right: Item) return Boolean is
  begin
    return Left.Value > Right.Value;
  end ">";

  -- Unit strings

  package Symbols is new Generic_Symbols;

  function Image (X: Dimension) return String renames Symbols.Image;

  procedure Construct (From_Unit_String: access procedure (C: out Character; EoT: out Boolean); Result: out Item; Length: out Natural) renames Symbols.Construct;

  package body Ensure_Task_Safety is
    -- Different tasks may concurrently evaluate unit strings. This
    -- generic avoids conflicts: By instantiating the package locally
    -- in functions, local variables become unique with each call.
    Last: Natural := Unit_String'First - 1;
    procedure Get (C: out Character; EoS: out Boolean) is
    begin
      EoS := Last = Unit_String'Last;
      if EoS then
        null;  -- don't consume
      else
        Last := Last + 1;
        C    := Unit_String (Last);
      end if;
    end Get;
  end Ensure_Task_Safety;

  function "*" (Left: Real'Base; Right: String) return Item is
  begin
    if Right = "" then  -- handle directly, FSM will fail
      return Left * One;
    end if;
    declare
      package String_Interface is new Ensure_Task_Safety ('*' & Right);  -- make task-safe
      R: Item;
      L: Positive;
    begin
      Construct (String_Interface.Get'Access, R, L);
      -- If we arrive here, syntax is OK, but there might be wrong characters left.
      if L - 1 /= Right'Length then  -- mind operator
        raise Illegal_Unit with "string not exhausted";
      end if;
      return Left * R;
    end;
  end "*";

  function "/" (Left: Real'Base; Right: String) return Item is
    package String_Interface is new Ensure_Task_Safety ("/(" & Right & ')');  -- add parentheses (there might be a product)
    R: Item;
    L: Positive;
  begin
    Symbols.Construct (String_Interface.Get'Access, R, L);
    return Left * R;
  end "/";

  -- Mathematics

  function Sqrt (X: Item) return Item is
  begin
    return (X.Unit ** (1/2),
            Sqrt (X.Value));
  end Sqrt;

  function Cbrt (X: Item) return Item is
  begin
    return (X.Unit  ** (1/3),
            X.Value ** to_Real (1/3));
  end Cbrt;

  function Log (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Log (X.Value));
  end Log;

  function Log (X, Base: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Log (X.Value, Base.Value));
  end Log;

  function Exp (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Exp (X.Value));
  end Exp;

  function Sin (X: Angle) return Dimensionless is
  begin
    return (uno,
            Sin (X.Value));
  end Sin;

  function Sin (X, Cycle: Item) return Dimensionless is
  begin
    return (uno,
            Sin (X.Value, Cycle.Value));
  end Sin;

  function Cos (X: Angle) return Dimensionless is
  begin
    return (uno,
            Cos (X.Value));
  end Cos;

  function Cos (X, Cycle: Item) return Dimensionless is
  begin
    return (uno,
            Cos (X.Value, Cycle.Value));
  end Cos;

  function Tan (X: Angle) return Dimensionless is
  begin
    return (uno,
            Tan (X.Value));
  end Tan;

  function Tan (X, Cycle: Item) return Dimensionless is
  begin
    return (uno,
            Tan (X.Value, Cycle.Value));
  end Tan;

  function Cot (X: Angle) return Dimensionless is
  begin
    return (uno,
            Cot (X.Value));
  end Cot;

  function Cot (X, Cycle: Item) return Dimensionless is
  begin
    return (uno,
            Cot (X.Value, Cycle.Value));
  end Cot;

  function Arcsin (X: Dimensionless) return Angle is
  begin
    return (uno,
            Arcsin (X.Value));
  end Arcsin;

  function Arcsin (X: Dimensionless; Cycle: Item) return Item is
  begin
    return (Cycle.Unit,
            Arcsin (X.Value, Cycle.Value));
  end Arcsin;

  function Arccos (X: Dimensionless) return Angle is
  begin
    return (uno,
            Arccos (X.Value));
  end Arccos;

  function Arccos (X: Dimensionless; Cycle: Item) return Item is
  begin
    return (Cycle.Unit,
            Arccos (X.Value, Cycle.Value));
  end Arccos;

  function Arctan (Y: Item;
                   X: Item := One) return Angle is
  begin
    return (uno,
            Arctan (Y.Value, X.Value));
  end Arctan;

  function Arctan (Y    : Item;
                   X    : Item := One;
                   Cycle: Item) return Item is
  begin
    return (Cycle.Unit,
            Arctan (Y.Value, X.Value, Cycle.Value));
  end Arctan;

  function Arccot (X: Item;
                   Y: Item := One) return Angle is
  begin
    return (uno,
            Arccot (X.Value, Y.Value));
  end Arccot;

  function Arccot (X    : Item;
                   Y    : Item := One;
                   Cycle: Item) return Item is
  begin
    return (Cycle.Unit,
            Arccot (X.Value, Y.Value, Cycle.Value));
  end Arccot;

  function Sinh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Sinh (X.Value));
  end Sinh;

  function Cosh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Cosh (X.Value));
  end Cosh;

  function Tanh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Tanh (X.Value));
  end Tanh;

  function Coth (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Coth (X.Value));
  end Coth;

  function Arcsinh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Arcsinh (X.Value));
  end Arcsinh;

  function Arccosh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Arccosh (X.Value));
  end Arccosh;

  function Arctanh (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Arctanh (X.Value));
  end Arctanh;

  function Arccoth (X: Dimensionless) return Dimensionless is
  begin
    return (uno,
            Arccoth (X.Value));
  end Arccoth;

end Generic_SI;
