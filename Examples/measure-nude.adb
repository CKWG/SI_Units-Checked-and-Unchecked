------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
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
-- Source:
-- http://???.html
--
-- Author's email address:
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;

separate (Measure)
package body Nude is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      9 April 2025
  --====================================================================
  -- Just the nude data.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  09.04.2025 New
  --====================================================================

  use Ada.Numerics;

  package Math is new Generic_Elementary_Functions (Float);
  use Math;

  Elementary_Charge: constant Float := 1.602_176_634E-19;
  Electron_Mass    : constant Float := 9.109_382_6E-31;
  c                : constant Float := 2.997_924_58E+8;
  Mu_0             : constant Float := 4.0E-7*Pi;
  Eps_0            : constant Float := 1.0/(Mu_0*c**2);

  function Schottky_Langmuir (Volt, Dist: Float) return Float is
  begin
    return (4.0/9.0) * Eps_0 * Sqrt (2.0 * Elementary_Charge / Electron_Mass) *
           Volt**1.5 / Dist**2;
  end Schottky_Langmuir;

end Nude;
