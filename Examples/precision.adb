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
-- As a special exception, if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Author's email address:
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
use  Ada.Text_IO;

with Generic_SI.Generic_Natural_Constants, Generic_SI.Generic_Text_IO;
with True_Dimensions, Dimension_Signature;
use  True_Dimensions;

procedure Precision is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      28 April 2026
  --====================================================================
  -- This test shows that at least 15 digits should be used for good
  -- precision.
  -- Compare the output with the exact values given in Wikipedia.
  --
  -- Problem: Discrepancy of Ada's values from defining equations
  --          with Wikipedia's numeric values.
  -- Alpha: 7.29735256530521E-3  (digits 15)  1/137.035999158713
  --        7.2973525643E-3      (Wikipedia)  1/137.035999177
  -- Diff:  1.00521E-12
  -- c, e_e, h values identical. Thus reason is discrepancy of Eps_0:
  -- Eps_0: 8.85418781762038985E-12*F/m  (digits 15)
  --        8.854187817E-12*F/m          (Wikipedia)
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  28.04.2026
  --====================================================================

  -- From Gnat package Standard
  type Float_06 is digits  6 range -16#0.FFFF_FF#E32               .. 16#0.FFFF_FF#E32;                for Float_06'Size use  32;
  type Float_15 is digits 15 range -16#0.FFFF_FFFF_FFFF_F8#E256    .. 16#0.FFFF_FFFF_FFFF_F8#E256;     for Float_15'Size use  64;
  type Float_18 is digits 18 range -16#0.FFFF_FFFF_FFFF_FFFF#E4096 .. 16#0.FFFF_FFFF_FFFF_FFFF#E4096;  for Float_18'Size use 128;

  generic
    type Real is digits <>;
  package World is
    package RIO is new Ada.Text_IO.Float_IO (Real'Base);
    package EF is new Ada.Numerics.Generic_Elementary_Functions (Real'Base);
    package DS is new Dimension_Signature (Dimension, uno);
    package SI is new Generic_SI (Real, EF, DS);
    package NC is new SI.Generic_Natural_Constants;
    package IO is new SI.Generic_Text_IO (RIO);
    procedure Speed_of_Light;
    procedure Charge;
    procedure Permittivity;
    procedure Permeability;
    procedure Planck;
    procedure Fine_Structure;
    procedure Fine_Inverse;
  end World;

  package body World is separate;

  package World_06 is new World (Float_06);
  package World_15 is new World (Float_15);
  package World_18 is new World (Float_18);

begin

  Put ("c");      World_06.Speed_of_Light;  World_15.Speed_of_Light;  World_18.Speed_of_Light;  Set_Col (10);  Put ("2.99792458E+8*m/s");    New_Line (2);
  Put ("e_e");    World_06.Charge;          World_15.Charge;          World_18.Charge;          Set_Col (10);  Put ("1.602176634E-19*C");    New_Line (2);
  Put ("Eps_0");  World_06.Permittivity;    World_15.Permittivity;    World_18.Permittivity;    Set_Col (10);  Put ("1.256637062E-06*H/m");  New_Line (2);
  Put ("Mu_0");   World_06.Permeability;    World_15.Permeability;    World_18.Permeability;    Set_Col (10);  Put ("8.854187817E-12*F/m");  New_Line (2);
  Put ("h");      World_06.Planck;          World_15.Planck;          World_18.Planck;          Set_Col (10);  Put ("6.62607015E-34*J*s");   New_Line (2);
  Put ("Alpha");  World_06.Fine_Structure;  World_15.Fine_Structure;  World_18.Fine_Structure;  Set_Col (10);  Put ("7.2973525643E-3");      New_Line;
                  World_06.Fine_Inverse;    World_15.Fine_Inverse;    World_18.Fine_Inverse;    Set_Col (10);  Put ("1.37035999177E+02");    New_Line;

end Precision;
