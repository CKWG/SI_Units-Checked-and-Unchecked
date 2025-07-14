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
--   http://home.T-Online.de/home/Christ-Usch.Grein/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Strings.Fixed, Ada.Text_IO;
use  Ada.Strings.Fixed, Ada.Text_IO;
use  Ada.Strings;

with SI.VS, SI.IO;
use  SI.VS, SI.IO, SI;

procedure Polar_Vectors is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      3 August 2018
  --====================================================================
  -- Test the polar vector operations.
  -- A vector moves on the surface of a sphere from the north pole to
  -- the south pole along the negative x-axis. This is done twice, once
  -- shifted a bit to positive y (longitude near +Pi), once to negative
  -- y (longutude near -Pi).
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  03.02.2006
  --  C.G.    2.0  16.02.2006 New definition of Polar vectors
  --  C.G.    2.1  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    3.0  03.08.2018 Unit strings
  --====================================================================

  Eps: constant Float := 0.00001;

  R: array (integer range <>) of Proto_Vector :=  -- When normalized:
      (-2 => (-Eps, Eps/2.0,  1.0),               -- north pole (appr.)
       -1 => (-0.5, Eps/2.0,  0.5),               -- northern hemisphere
        0 => (-1.0, Eps/2.0,  0.0),               -- equator
        1 => (-0.5, Eps/2.0, -0.5),               -- southern hemisphere
        2 => (-Eps, Eps/2.0, -1.0));              -- south pole (approx.)
  Text: array (R'Range) of String (1 .. 21) :=
    (-2 => "  north pole         ",
     -1 => "  northern hemisphere",
      0 => "  equator            ",
      1 => "  southern hemisphere",
      2 => "  south pole         ");

  P: Polar;

begin

  Default_Fore := 2;
  Default_Aft  := 2;
  Default_Exp  := 0;

  Put_Line ("Move unit vector from north pole to south pole along the negative");
  Put_Line ("x-axis. Longitude near +Pi and -Pi.");
  New_Line;

  for e in 0 .. 1 loop

    Put_Line (" Radius  Longitude  Latitude");
    New_Line;

    for i in R'Range loop
      R (i)(2) := R (i)(2) - Float (e) * Eps;
      P := To_Polar (Normalize (R (i)) * (1.0*"m"));
      Put (Radius    (P));     Put ("  ");
      Put (Longitude (P)/Pi);  Put ("*Pi  ");
      Put (Latitude  (P)/Pi);  Put ("*Pi");
      Put_Line (Text (i));
    end loop;

    New_Line;

  end loop;

  Put_Line ("        Longitude");
  Put_Line ("      +Pi       -Pi");
  New_Line;

  for Lat in Text'Range loop
    for I in Axis loop
      P := Compose_Polar (1.0*"kN", Phi => +(Pi-Eps)*"rad", Theta => Float (Lat + 2)*Pi/4.0*"rad");  -- on negative x-axis
      for Long in 0 .. 1 loop  -- Longitude +Pi .. -Pi
        case Long is
          when 0 => Put ("(");  Put (Trim (Axis'Image (I), Left));  Put (") ");
          when 1 => Set_Longitude (P, -(Pi-Eps)*"rad");  Put ("  ");
        end case;
        Put (Get (V => To_Cartesian (P), A => I), Dim => "kN");
      end loop;
      Put_Line (if I = Axis'First then Text (Lat) else "");
    end loop;
    New_Line;
  end loop;

end Polar_Vectors;
