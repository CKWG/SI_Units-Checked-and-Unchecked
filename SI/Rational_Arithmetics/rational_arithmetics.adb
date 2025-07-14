------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2005, 2006, 2018 Christoph Karl Walter Grein
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
-- Author's homepage and email address:
--   http://www.christ-usch-grein.homepage.t-online.de/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

package body Rational_Arithmetics is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      27 February 2006
  --====================================================================
  -- Because we've made some operators abstract, we've to use some type
  -- conversions to obtain the needed operations. They will not produce
  -- any machine code.
  -- The implementation guarantees that rational numbers are valid or
  -- else Constraint_Error will be raised.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.06.2002
  --  C.G.    1.1  05.07.2002 added mixed relational operators,
  --                          Image and Value
  --  C.G.    2.0  15.11.2005 Ada 2005
  --  C.G.    2.1  27.11.2005 Denominator returns Positive_Whole
  --  C.G.    2.2  01.02.2006 Bug fix in Value
  --  C.G.    3.0  27.02.2006 pragma Pure: Put Image and Value in child
  --====================================================================

  function GCD (Left, Right: Whole) return Integer is
    L: Whole := Left;
    R: Whole := Right;
  begin
    while R /= 0 loop
      L := L mod R;
      if L = 0 then
        return Integer (R);
      end if;
      R := R mod L;
    end loop;
    return Integer (L);
  end GCD;

  function Reduce (X: Rational) return Rational is
  begin
    if X.Denominator = 0 then
      raise Constraint_Error;
    elsif X.Numerator = 0 then
      return (0, 1);
    end if;
    declare
      G: constant Integer := GCD (X.Numerator, X.Denominator);
    begin
      return (Whole (Integer (X.Numerator  ) / G),
              Whole (Integer (X.Denominator) / G));
    end ;
  end Reduce;

  function Numerator (R: Rational) return Whole is
  begin
    return R.Numerator;
  end Numerator;

  function Denominator (R: Rational) return Positive_Whole is
  begin
    return R.Denominator;
  end Denominator;

  function "+" (Right: Whole) return Rational is
  begin
    return (Right, 1);
  end "+";

  function "-" (Right: Whole) return Rational is
  begin
    return (Whole (-Integer (Right)), 1);
  end "-";

  function "/" (Left, Right: Whole) return  Rational is
    L:          Integer :=     Integer (Left);
    R: constant Integer := abs Integer (Right);
  begin
    if Right < 0 then
      L := -L;
    end if;
    return Reduce ((Whole (L), Whole (R)));
  end "/";

  function "abs" (Right: Rational) return Rational is
  begin
    return (abs Right.Numerator, Right.Denominator);
  end "abs";

  function "+" (Right: Rational) return Rational is
  begin
    return Right;
  end "+";

  function "-" (Right: Rational) return Rational is
  begin
    return (Whole (-Integer (Right.Numerator)), Right.Denominator);
  end "-";

  function "+" (Left, Right: Rational) return Rational is
    R: constant Rational :=
         (Left.Numerator   * Right.Denominator + Left.Denominator * Right.Numerator,
          Left.Denominator * Right.Denominator);
  begin
    return Reduce (R);
  end "+";

  function "-" (Left, Right: Rational) return Rational is
    R: constant Rational :=
         (Left.Numerator   * Right.Denominator - Left.Denominator * Right.Numerator,
          Left.Denominator * Right.Denominator);
  begin
    return Reduce (R);
  end "-";

  function "*" (Left, Right: Rational) return Rational is
    R: constant Rational := (Left.Numerator   * Right.Numerator  ,
                             Left.Denominator * Right.Denominator);
  begin
    return Reduce (R);
  end "*";

  function "/" (Left, Right: Rational) return Rational is
    L:          Rational :=     Left ;
    R: constant Rational := abs Right;
  begin
    if Right.Numerator < 0 then
      L.Numerator := Whole (-Integer (L.Numerator));
    end if;
    return Reduce ((L.Numerator   * R.Denominator,
                    L.Denominator * R.Numerator  ));
  end "/";

  function "=" (Left: Whole; Right: Rational) return Boolean is
  begin
    return Left = Right.Numerator and Right.Denominator = 1;
  end "=";

  function "=" (Left: Rational; Right: Whole) return Boolean is
  begin
    return Right = Left;
  end "=";

  function "<" (Left, Right: Rational) return Boolean is
  begin
    return Left.Numerator * Right.Denominator < Left.Denominator * Right.Numerator;
  end "<";

  function "<=" (Left, Right: Rational) return Boolean is
  begin
    return Left.Numerator * Right.Denominator <= Left.Denominator * Right.Numerator;
  end "<=";

  function ">=" (Left, Right: Rational) return Boolean is
  begin
    return Left.Numerator * Right.Denominator >= Left.Denominator * Right.Numerator;
  end ">=";

  function ">" (Left, Right: Rational) return Boolean is
  begin
    return Left.Numerator * Right.Denominator > Left.Denominator * Right.Numerator;
  end ">";

  function "<" (Left: Whole; Right: Rational) return Boolean is
  begin
    return +Left < Right;
  end "<";

  function "<" (Left: Rational; Right: Whole) return Boolean is
  begin
    return Left < +Right;
  end "<";

  function "<=" (Left: Whole; Right: Rational) return Boolean is
  begin
    return +Left <= Right;
  end "<=";

  function "<=" (Left: Rational; Right: Whole) return Boolean is
  begin
    return Left <= +Right;
  end "<=";

  function ">=" (Left: Whole; Right: Rational) return Boolean is
  begin
    return +Left >= Right;
  end ">=";

  function ">=" (Left: Rational; Right: Whole) return Boolean is
  begin
    return Left >= +Right;
  end ">=";

  function ">" (Left: Whole; Right: Rational) return Boolean is
  begin
    return +Left > Right;
  end ">";

  function ">" (Left: Rational; Right: Whole) return Boolean is
  begin
    return Left > +Right;
  end ">";

  function Max (Left, Right: Rational) return Rational is
  begin
    if Left >= Right then
      return Left;
    else
      return Right;
    end if;
  end Max;

  function Min (Left, Right: Rational) return Rational is
  begin
    if Left <= Right then
      return Left;
    else
      return Right;
    end if;
  end Min;

  function Max (Left: Whole; Right: Rational) return Rational is
  begin
    return Max (+Left, Right);
  end Max;

  function Max (Left: Rational; Right: Whole) return Rational is
  begin
    return Max (Left, +Right);
  end Max;

  function Min (Left: Whole; Right: Rational) return Rational is
  begin
    return Min (+Left, Right);
  end Min;

  function Min (Left: Rational; Right: Whole) return Rational is
  begin
    return Min (Left, +Right);
  end Min;

  function "+" (Left: Whole; Right: Rational) return Rational is
    R: constant Rational := (Left * Right.Denominator + Right.Numerator,
                             Right.Denominator);
  begin
    return Reduce (R);
  end "+";

  function "+" (Left: Rational; Right: Whole) return Rational is
  begin
    return Right + Left;
  end "+";

  function "-" (Left: Whole; Right: Rational) return Rational is
    R: constant Rational := (Left*Right.Denominator - Right.Numerator,
                             Right.Denominator);
  begin
    return Reduce (R);
  end "-";

  function "-" (Left: Rational; Right: Whole) return Rational is
    R: constant Rational := (Left.Numerator - Right * Left.Denominator,
                             Left.Denominator);
  begin
    return Reduce (R);
  end "-";

  function "*" (Left: Whole; Right: Rational) return Rational is
    R: constant Rational := (Left * Right.Numerator,
                             Right.Denominator);
  begin
    return Reduce (R);
  end "*";

  function "*" (Left: Rational; Right: Whole) return Rational is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Whole; Right: Rational) return Rational is
    R: constant Rational := (Left * Right.Denominator,
                             Right.Numerator);
  begin
    return Reduce (R);
  end "/";

  function "/" (Left: Rational; Right: Whole) return Rational is
    R: constant Rational := (Left.Numerator,
                             Left.Denominator * Right);
  begin
    return Reduce (R);
  end "/";

end Rational_Arithmetics;
