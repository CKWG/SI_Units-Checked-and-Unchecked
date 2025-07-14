------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2006, 2018 Christoph Karl Walter Grein
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

package Rational_Arithmetics.Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      29 July 2018
  --====================================================================
  -- Image returns the argument as a string.
  -- If the denominator is 1, the string has the format "sn", else it
  -- has the format "sn/m" where there is no white space around '/',
  -- and the sign 's' is blank for non-negative numbers.
  -- Value returns the argument into a rational number ignoring any
  -- leading or trailing blanks.
  -- It raises Constraint_Error if the string does not have the
  -- aforementioned format.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  27.02.2006
  --  C.G.    1.0  29.07.2018 Preelaborate
  --====================================================================

  pragma Preelaborate;
  pragma Elaborate_Body;

  function Image (X: Rational) return String;
  function Value (X: String  ) return Rational;

end Rational_Arithmetics.Strings;
