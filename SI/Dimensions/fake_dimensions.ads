------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2020, 2025 Christoph Karl Walter Grein
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

with Rational_Arithmetics;
use  Rational_Arithmetics;

package Fake_Dimensions is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      13 April 2025
  --====================================================================
  -- This is the package to use for instantiation of Generic_SI when
  -- dimention checking is not intended.
  -- Make sure that pragma Assertion_Policy (Ignore); is set.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    2.1  13.04.2025 Replace pragma Inline by aspect
  --  C.G.    2.0  06.04.2025 Replace +0 by constant; hide in private
  --  C.G.    1.0  11.05.2020
  --====================================================================

  type Dimension is null record;

  uno: constant Dimension := (null record);

  function m   return Dimension is (uno) with Inline;
  function kg  return Dimension is (uno) with Inline;
  function s   return Dimension is (uno) with Inline;
  function A   return Dimension is (uno) with Inline;
  function K   return Dimension is (uno) with Inline;
  function cd  return Dimension is (uno) with Inline;
  function mol return Dimension is (uno) with Inline;

  function "*" (Left, Right: Dimension) return Dimension is (uno) with Inline;
  function "/" (Left, Right: Dimension) return Dimension is (uno) with Inline;

  function "**" (Base: Dimension; Exponent: Whole   ) return Dimension is (uno) with Inline;
  function "**" (Base: Dimension; Exponent: Rational) return Dimension is (uno) with Inline;

  -- Access to the components

  function m   (X: Dimension) return Rational with Inline;
  function kg  (X: Dimension) return Rational with Inline;
  function s   (X: Dimension) return Rational with Inline;
  function A   (X: Dimension) return Rational with Inline;
  function K   (X: Dimension) return Rational with Inline;
  function cd  (X: Dimension) return Rational with Inline;
  function mol (X: Dimension) return Rational with Inline;

private

  Zero: constant Rational := +0;

  function m   (X: Dimension) return Rational is (Zero);
  function kg  (X: Dimension) return Rational is (Zero);
  function s   (X: Dimension) return Rational is (Zero);
  function A   (X: Dimension) return Rational is (Zero);
  function K   (X: Dimension) return Rational is (Zero);
  function cd  (X: Dimension) return Rational is (Zero);
  function mol (X: Dimension) return Rational is (Zero);

end Fake_Dimensions;
