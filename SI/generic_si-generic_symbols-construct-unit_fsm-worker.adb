------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2025 Christoph Karl Walter Grein
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

separate (Generic_SI.Generic_Symbols.Construct.Unit_FSM)
package body Worker is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      15 September 2025
  --====================================================================
  -- Unit, the prefix and symbol, is created character per character;
  -- it might be correct if Read (the number of characters read from the
  -- input) is not greater than Max.
  -- Evaluate_Symbol constructs from Unit the Ident (Value and
  -- Dimension).
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    0.0  22.07.2025 Raw design
  --  C.G.    0.1  27.07.2025 FSM structure works
  --  C.G.    0.2  03.08.2025 Integrated in SI hierarchy;
  --                          works without exponents
  --  C.G.    0.3  11.08.2025 Works for unit strings
  --  C.G.    1.0  21.08.2025 Final touch
  --  C.G.    1.1  15.09.2025 Allow µ
  --====================================================================

  Ident: Item := One;  -- the identifier in the syntax

  Denom: Boolean := False;

  Max: constant Positive := 5;  -- maximum Symbol length, e.g. "damol"

  Read: Natural := 0;  -- Symbol in Unit_String might erroneously be longer than Max letters
  Unit: String (1 .. Max) := (others => ' ');
  U   : Character;  -- potential character for Unit

  function Next_Character return Class is
    EoI: Boolean;
  begin
    From_Unit_String (U, EoI);
    if EoI then
      return EoT;
    end if;
    Length := Length + 1;
    case U is
      when ' ' => Length := Length - 1;  -- must not be consumed
                  return EoT;
      when '*' => return '*';
      when '/' => return '/';
      when '(' => return '(';
      when '+' |
           '-' => return Sign;
      when ')' => return ')';
      when '0' .. '9' => return Digit;
      when 'A' .. 'Z' | 'a' .. 'z' | 'µ' => return Letter;
      when others => Length := Length - 1;  -- must not be consumed
                     return EoT;
    end Case;
  end Next_Character;

  procedure Set_Denominator is
  begin
    Denom := True;
  end Set_Denominator;

  procedure Take_Letter is
  begin
    Read := Read + 1;
    Unit (Integer'Min (Read, Max)) := U;
  end Take_Letter;

  procedure Evaluate_Symbol is
  begin
    if Read > 5 then
      raise Illegal_Unit with "Symbol too long";
    end if;
    Ident := Evaluate (Unit (1 .. Read));
    if Denom then
      Ident := 1.0 / Ident;
    end if;
    Read  := 0;                -- prepare next symbol
    Unit  := (others => ' ');  -- unnecessary, but facilitates error hunting
  end Evaluate_Symbol;

  procedure No_Exponent is
  begin
    Factor := Factor * Ident;
  end No_Exponent;

  --------------------------------------------------------------------

  Number, Frac: Whole := 0;
  Minus: Boolean  := False;

  procedure Signed_Exponent is
  begin
    Minus := U = '-';
  end Signed_Exponent;

  procedure Take_Digit is
  begin
    Number := 10 * Number + Whole'Value ((1 => U));
  end Take_Digit;

  procedure Evaluate_Whole (Fraction: Boolean) is
  begin
    if Minus then
      Number := -Number;
      Minus  := False;
    end if;
    if Fraction then
      Frac   := Number;  -- Numerator
      Number := 0;       -- Denominator to be read
    end if;
  end Evaluate_Whole;

  procedure Evaluate_Simple_Exponent is
  begin
    Ident  := Ident ** Number;
    Factor := Factor * Ident;
    Number := 0;
  end Evaluate_Simple_Exponent;

  procedure Evaluate_Exponent is
  begin
    Ident  := Ident ** (Frac / Number);
    Factor := Factor * Ident;
    Number := 0;
  exception
    when Constraint_Error => raise Illegal_Unit with "wrong exponent";
  end Evaluate_Exponent;

  --------------------------------------------------------------------

  procedure Create_Result is
  begin
    Result := Result * Factor;
    Denom := False;
  end Create_Result;

end Worker;
