------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2019, 2020 Christoph Karl Walter Grein
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
-- Source:
-- https://www.adaic.org/ada-resources/tools-libraries/
--   (see Christoph Grein's Essentials)
-- http://archive.adaic.com/tools/CKWG/Dimension/Dimension.html
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Strings.Maps;

separate (Generic_SI.Value)
procedure Get_Unit_Factor (X: String; Last: out Natural; Factor: out Item) is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      10 May 2020
  --====================================================================
  -- Remember, X is not empty.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  28.07.2018
  --  C.G.    1.1  29.09.2018 Unit syntax changed
  --  C.G.    2.0  15.02.2019 subpackage Symbols inserted
  --  C.G.    3.0  10.05.2020 Moved from Generic_Strings
  --====================================================================

  Pos: Positive := X'First;

  function Get_Symbol return Item is
    -- Longest symbol is "mol", longest prefix is "da", thus evaluate
    -- at most 5 characters. Any characters left will be dealt with in caller.
    -- At return, Pos is the next unevaluated character in Expression.
    Symbol: String (1 .. 5);
    Taken : Natural := 0;
    use Ada.Strings.Maps;
  begin
    for S in Symbol'Range loop
      exit when Pos > X'Last  -- beyond X
           or else (Pos /= X'First and X (Pos) in '*' | '/' | ')');  -- next symbol starts or denominator ends
      if not Is_In (X (Pos), Symbols.Legal_Characters) then
        raise Illegal_Unit;
      end if;
      Taken      := S;
      Symbol (S) := X (Pos);
      Pos        := Pos + 1;
    end loop;
    return Symbols.Evaluate (Symbol (1 .. Taken));
  end Get_Symbol;

  function Get_Whole return Whole is
    -- At start, Pos is the first character to evaluate.
    -- At return, Pos is the next unevaluated character.
    B: Positive := Pos;  -- might be '+' or '-'
  begin
    loop
      Pos := Pos + 1;
      exit when Pos > X'Last or else X (Pos) not in '0' .. '9';
    end loop;
    return Whole'Value (X (B .. Pos - 1));  -- might raise exception
  end Get_Whole;

  function Get_Rational return Rational is
    -- At start, Pos is the first character to evaluate.
    -- At return, Pos is the next unevaluated character.
    N: Whole renames Get_Whole;
  begin
    if Pos <= X'Last and then X (Pos) = '/' then
      Pos := Pos + 1;  -- consume '/'
      if X (Pos) in '+' | '-' then
        raise Illegal_Unit;
      end if;
      return N/Get_Whole;
    else
      return +N;
    end if;
  end Get_Rational;

  function Get_Exponent return Rational is
    -- There is an exponent if the next two characters are "**".
    -- At start, Pos is the first character to evaluate.
    -- At return, Last is correctly set.
    Exponent   : Rational;
    Parentheses: Boolean;
  begin
    -- Is there an exponent?
    if Pos > X'Last then  -- beyond X
      Last := X'Last;
      return +1;
    elsif Pos + 1 > X'Last or else X (Pos .. Pos + 1) /= "**" then
      Last := Pos - 1;
      return +1;
    end if;
    -- There is an exponent.
    Pos := Pos + 2;  -- consume "**"
    -- Is there a parenthesis?
    Parentheses := X (Pos) = '(';
    if Parentheses then
      Pos := Pos + 1;  -- consume '('
    elsif X (Pos) in '+' | '-' then
      raise Illegal_Unit;
    end if;
    if Parentheses then  -- m**(1/3)
      Exponent := Get_Rational;
      if X (Pos) /= ')' then
        raise Illegal_Unit;
      end if;
      Last := Pos;  -- consume ')'
    else  -- only whole number may follow, e.g. m**2/s**2
      Exponent := +Get_Whole;
      Last     := Pos - 1;
    end if;
    return Exponent;
  exception
    when Constraint_Error =>  -- beyond Expression or illegal characters
      raise Illegal_Unit;
  end Get_Exponent;

  Symbol  : Item     renames Get_Symbol  ;  -- force sequence
  Exponent: Rational renames Get_Exponent;  -- of evaluation

begin

  Factor := Symbol ** Exponent;

end Get_Unit_Factor;
