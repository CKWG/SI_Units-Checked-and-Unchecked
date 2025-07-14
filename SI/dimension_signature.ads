------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2020 Christoph Karl Walter Grein
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

generic

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      11 May 2020
  --====================================================================
  -- Signature package for Generic_SI defining the dimension handling.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  11.05.2020
  --====================================================================

  type Dimension is private;

  uno: Dimension;

  with function m   return Dimension is <>;
  with function kg  return Dimension is <>;
  with function s   return Dimension is <>;
  with function A   return Dimension is <>;
  with function K   return Dimension is <>;
  with function cd  return Dimension is <>;
  with function mol return Dimension is <>;

  with function "*" (Left, Right: Dimension) return Dimension is <>;
  with function "/" (Left, Right: Dimension) return Dimension is <>;

  with function "**" (Base: Dimension; Exponent: Whole   ) return Dimension is <>;
  with function "**" (Base: Dimension; Exponent: Rational) return Dimension is <>;

  -- Access to the components

  with function m   (X: Dimension) return Rational is <>;
  with function kg  (X: Dimension) return Rational is <>;
  with function s   (X: Dimension) return Rational is <>;
  with function A   (X: Dimension) return Rational is <>;
  with function K   (X: Dimension) return Rational is <>;
  with function cd  (X: Dimension) return Rational is <>;
  with function mol (X: Dimension) return Rational is <>;

package Dimension_Signature is
end Dimension_Signature;
