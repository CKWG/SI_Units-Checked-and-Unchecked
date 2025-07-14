------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2006, 2018, 2020 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

generic
package Generic_SI.Generic_Polynomials is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   5.0
  -- Date      14 May 2020
  --====================================================================
  -- Polynomial defines the function
  --    f(x) := sum (Coeff (i) * X**i)
  -- Coefficients' range need not include 0, e.g.
  --    Coefficients = (1=> 1.0, 2=> 1.0)
  -- defines the term x*(x+1).
  --
  -- Linear_Regression defines the coefficients for a linear function
  -- approximating a sample of points. The sample S must at least hold
  -- two points, else Sample_Error is raised.
  --
  -- Define creates a table for linear interpolation. The sample S must
  -- at least hold two points, else Sample_Error is raised. The length
  -- of the table returned is S'Length.
  -- X and Y denote the abscissa and ordinate values of sample points.
  -- X must be strictly monotonic (increasing or decreasing) or else
  -- Sample_Error is raised.
  -- Linear_Interpolation interpolates linearly for x within the Table
  -- range and extrapolates outside.
  -- Table_Error is propagated if the table is undefined.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  07.12.1994
  --  C.G.    1.1  13.05.1997 Interface changed
  --  C.G.    2.0  31.07.1997 Interpolation table made private
  --  C.G.    3.0  26.07.2002 Adapted to SI package
  --  C.G.    3.1  26.02.2006 pragma Pure
  --  C.G.    4.0  26.08.2018 Unit strings
  --  C.G.    5.0  14.05.2020 Dimensions generic parameter
  --====================================================================

  -- Polynomials

  type Coefficients is array (Natural_Whole range <>) of Item;
  subtype Straight_Line is Coefficients (0 .. 1);

  function Polynomial (Coeff: Coefficients; X: Item) return Item;

  -- Linear Regression and Interpolation

  type Pair is record
    X, Y: Item;  -- y=f(X)
  end record;

  type Sample is array (Positive range <>) of Pair;

  function Linear_Regression (S: Sample) return Straight_Line;

  type Interpolation_Table (Length: Positive) is private;
  function Define (S: Sample) return Interpolation_Table;

  function Linear_Interpolation
               (Table: Interpolation_Table; X: Item) return Item;

  Sample_Error: exception;

private

  type Interpolation_Table (Length: Positive) is record
    Sign: Real := 0.0;  -- means undefined
    S: Sample (1 .. Length);
  end record;

end Generic_SI.Generic_Polynomials;
