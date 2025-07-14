------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2004, 2006, 2018 Christoph Karl Walter Grein
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

with SI.VS, SI.IO;
use  SI.VS, SI.IO, SI;

procedure Matrix_Inversion is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      3 August 2018
  --====================================================================
  -- Test the matrix inversion.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  08.11.2002
  --  C.G.    2.0  29.03.2004 Adapted to new interface
  --  C.G.    2.1  31.01.2006 Use unused declaration
  --  C.G.    2.2  27.02.2006 Fixed bug introduced with version 2.1
  --  C.G.    2.3  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    3.0  03.08.2018 Unit strings
  --====================================================================

  M: constant Matrix := (( 3.0,  2.0,  1.0),
                         ( 1.0,  0.0,  2.0),
                         ( 4.0,  1.0,  3.0)) * One;
  N: constant Matrix := ((-0.4, -1.0,  0.8),
                         ( 1.0,  1.0, -1.0),
                         ( 0.2,  1.0, -0.4)) * One;
  D: constant Item   := Det (M);
  I: constant Matrix := Inv (M);
  R: constant Matrix := Inv (N);

begin

  Put_Line ("Det (M) = 5 => " & Boolean'Image (D = 5.0 * One));  -- True (rounding errors?)
  Put_Line ("Inv (M) = N => " & Boolean'Image (I = N));          -- True (rounding errors?)
  Put_Line ("Inv (N) = M => " & Boolean'Image (R = M));          -- True (rounding errors?)

  New_Line;
  Put_Line ("Inv (N) =>");
  for I in Axis loop
    for J in Axis loop
      Put (Get (R, I, J), Aft => 0, Exp => 0);
    end loop;
    New_Line;
  end loop;

end Matrix_Inversion;
