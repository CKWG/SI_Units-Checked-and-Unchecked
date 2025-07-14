------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2006, 2008, 2018, 2020 Christoph Karl Walter Grein
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

with Ada.Text_IO;
use  Ada.Text_IO;

with SI.IO, SI.VS;
use  SI.IO, SI.VS, SI;

procedure Vector_Algebra is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      24 February 2020
  --====================================================================
  -- Vector algebra: Force on a charge in an electomagnetic field.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  17.11.2002
  --  C.G.    1.1  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    1.2  03.04.2008 Unit names Volt_p_Meter, Meter_p_Second
  --                          do no longer exist
  --  C.G.    2.0  03.08.2018 Unit strings
  --  C.G.    2.1  24.02.2020 Use the new vector subtypes
  --====================================================================

  Q: Charge := 1.0*"C";

  E: Electric_Field_Vector := 3.0 * "V/m" * Unit_Vector (1);  -- some
  B: Magnetic_Field_Vector := 1.0 * "T" * U2;                 --   ways to
  V: Velocity              := P2 * (2.0 * "m/s");             --     construct
  F: Force_Vector;                                            --       vectors

begin

  F := Q * (E + Cross (V, B));

  for A in Axis loop
    Put (Get (F, A), Dim => "N");  New_Line;
  end loop;

  New_Line;

  for A in Axis loop
    Put (Inner (F, Unit_Vector (A)), Aft => 0, Exp => 0, Dim => "daN");  New_Line;
  end loop;

end Vector_Algebra;
