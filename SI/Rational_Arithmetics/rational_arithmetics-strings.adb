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

with Ada.Strings.Fixed;

package body Rational_Arithmetics.Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      27 February 2006
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  27.02.2006
  --====================================================================

  function Image (X: Rational) return String is
    N: constant String := Whole'Image (X.Numerator);
    D: constant String := Whole'Image (X.Denominator);
  begin
    if X.Denominator = 1 then
      return N;
    else
      return N & '/' & D (2 .. D'Last);
    end if;
  end Image;

  function Value (X: String) return Rational is
    use Ada.Strings;
    Y: String renames Fixed.Trim (X, Side => Both);
    -- Append '/' so that it is always found:
    S: Natural := Fixed.Index (Y & '/', "/");
    N: constant Whole:= Whole'Value (Y (Y'First .. S-1));
    D: Positive_Whole;
  begin
    if Y (S-1) = ' ' then  -- blank before /
      raise Constraint_Error;
    end if;
    if S > Y'Last then
      return (N, 1);
    elsif Y (S+1) = ' ' then  -- blank after /
      raise Constraint_Error;
    end if;
    D := Whole'Value (Y (S+1 .. Y'Last));
    return N/D;
  end Value;

end Rational_Arithmetics.Strings;
