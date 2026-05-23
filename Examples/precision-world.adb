------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2026 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

separate (Precision)
package body World is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      28 April 2026
  --====================================================================
  -- Comparison of Ada's and Wikipedia's values.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  28.04.2026
  --====================================================================

  use World.SI, World.NC, World.IO;

  procedure Speed_of_Light is
    Cc: constant Item := 1.0/Sqrt(Eps_0*Mu_0);
  begin
    Set_Col ( 9);  Put (c , Dim => "Mm/s");
    Set_Col (40);  Put (Cc, Dim => "m/s");
    Set_Col (70);  Put (c - Cc);
    New_Line;
  end Speed_of_Light;

  procedure Charge is
  begin
    Set_Col (9);  Put (e_e, Dim => "C");
  end Charge;

  procedure Permittivity is
  begin
    Set_Col (9);  Put (Eps_0, Dim => "F/m");
  end Permittivity;

  procedure Permeability is
  begin
    Set_Col (9);  Put (Mu_0, Dim => "H/m");
  end Permeability;

  procedure Planck is
  begin
    Set_Col (9);  Put (h, Dim => "J*s");
  end Planck;

  procedure Fine_Structure is
    A_Inv: constant Dimensionless := 2.0*Eps_0*h*c/e_e**2;
  begin
    Set_Col ( 9);  Put (Alpha);
    Set_Col (40);  Put (1.0/A_Inv);
    Set_Col (70);  Put (Alpha - 1.0/A_Inv);
  end Fine_Structure;

  procedure Fine_Inverse is
    A_Inv: constant Dimensionless := 2.0*Eps_0*h*c/e_e**2;
  begin
    Set_Col ( 9);  Put (1.0/Alpha);
    Set_Col (40);  Put (A_Inv);
    Set_Col (70);  Put (1.0/Alpha - A_Inv);
    New_Line;
  end Fine_Inverse;

end World;
