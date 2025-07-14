------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2005, 2006, 2009, 2018 Christoph Karl Walter Grein
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

package Rational_Arithmetics is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.1
  -- Date      24 July 2009
  --====================================================================
  -- Define rational (n/m) and mixed (i + n/m) numbers.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.06.2002
  --  C.G.    1.1  05.07.2002 added mixed relational operators,
  --                          Image and Value
  --  C.G.    2.0  14.06.2005 Ada 2006 RM 6.4(8/2), AARM 6.4(31.d/2):
  --                          abstract operations are undefined
  --                          (hence package Invisible could be removed)
  --  C.G.    2.1  27.11.2005 Denominator returns Positive_Whole
  --  C.G.    3.0  27.02.2006 pragma Pure: Put Image and Value in child
  --  C.G.    3.1  24.07.2009 overriding
  --====================================================================

  pragma Pure;
  pragma Elaborate_Body;

  -- Whole numbers

  type Whole is new Integer;
  subtype Natural_Whole  is Whole range 0 .. Whole'Last;
  subtype Positive_Whole is Whole range 1 .. Whole'Last;

  -- Undefine unwanted operations

  overriding function "+" (Right: Whole) return Whole is abstract;
  overriding function "-" (Right: Whole) return Whole is abstract;

  overriding function "/" (Left, Right: Whole) return Whole is abstract;

  -- Rational numbers

  type Rational is private;

  function Numerator   (R: Rational) return Whole;
  function Denominator (R: Rational) return Positive_Whole;

  -- Constructors

  function "+" (Right: Whole) return Rational;
  function "-" (Right: Whole) return Rational;

  function "/" (Left, Right: Whole) return Rational;

  -- Rational operations

  function "abs" (Right: Rational) return Rational;

  function "+" (Right: Rational) return Rational;
  function "-" (Right: Rational) return Rational;

  function "+" (Left, Right: Rational) return Rational;
  function "-" (Left, Right: Rational) return Rational;

  function "*" (Left, Right: Rational) return Rational;
  function "/" (Left, Right: Rational) return Rational;

  -- Relational

  function "=" (Left: Whole   ; Right: Rational) return Boolean;
  function "=" (Left: Rational; Right: Whole   ) return Boolean;

  function "<"  (Left, Right: Rational) return Boolean;
  function "<=" (Left, Right: Rational) return Boolean;
  function ">=" (Left, Right: Rational) return Boolean;
  function ">"  (Left, Right: Rational) return Boolean;

  function "<"  (Left: Whole   ; Right: Rational) return Boolean;
  function "<"  (Left: Rational; Right: Whole   ) return Boolean;
  function "<=" (Left: Whole   ; Right: Rational) return Boolean;
  function "<=" (Left: Rational; Right: Whole   ) return Boolean;
  function ">=" (Left: Whole   ; Right: Rational) return Boolean;
  function ">=" (Left: Rational; Right: Whole   ) return Boolean;
  function ">"  (Left: Whole   ; Right: Rational) return Boolean;
  function ">"  (Left: Rational; Right: Whole   ) return Boolean;

  function Max (Left, Right: Rational) return Rational;
  function Min (Left, Right: Rational) return Rational;

  function Max (Left: Whole   ; Right: Rational) return Rational;
  function Max (Left: Rational; Right: Whole   ) return Rational;
  function Min (Left: Whole   ; Right: Rational) return Rational;
  function Min (Left: Rational; Right: Whole   ) return Rational;

  -- Mixed operations

  function "+" (Left: Whole   ; Right: Rational) return Rational;
  function "+" (Left: Rational; Right: Whole   ) return Rational;
  function "-" (Left: Whole   ; Right: Rational) return Rational;
  function "-" (Left: Rational; Right: Whole   ) return Rational;

  function "*" (Left: Whole   ; Right: Rational) return Rational;
  function "*" (Left: Rational; Right: Whole   ) return Rational;
  function "/" (Left: Whole   ; Right: Rational) return Rational;
  function "/" (Left: Rational; Right: Whole   ) return Rational;

private

  pragma Inline (Numerator, Denominator,
                 "+", "-", "*", "/");

  type Rational is record
    Numerator  : Whole;
    Denominator: Positive_Whole;
  end record;

end Rational_Arithmetics;
