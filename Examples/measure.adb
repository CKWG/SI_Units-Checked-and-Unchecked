------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
-- Copyright (C) 2002, 2005, 2008, 2018, 2020, 2021, 2025
--               Christoph Karl Walter Grein
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

with Ada.Calendar;
use  Ada.Calendar;
with Ada.Text_IO;
use  Ada.Text_IO;

with Rational_Arithmetics;
use  Rational_Arithmetics;

with SI;

procedure Measure is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   5.0
  -- Date      15 June 2025
  --====================================================================
  -- Measurements of relative speeds.
  -- Schottky-Langmuir; Matrix and Quaternion rotations.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.07.2002 with work-around for Gnat 3.14p bug
  --  C.G.    1.1  27.09.2002 Gnat work-around removed for 3.16w
  --                          (there is one work-around left)
  --  C.G.    1.2  10.10.2005 Gnat GPL 2005: last work-around removed
  --  C.G.    1.3  23.11.2005 Added Put for result of Schottky_Langmuir
  --  C.G.    1.4  03.04.2008 Unit name Meter_2 does no longer exist
  --  C.G.    2.0  07.08.2018 Unit strings
  --  C.G.    3.0  15.05.2020 Dimensions generic parameter
  --  C.G.    4.0  04.10.2021 Measurement improved
  --  C.G.    5.0  15.06.2025 Reworked for Preconditions
  --====================================================================

  package Duration_Text_IO is new Ada.Text_IO.Fixed_IO (Duration);
  use Duration_Text_IO;

  package Nude is
    function Schottky_Langmuir (Volt, Dist: Float) return Float;
  end Nude;
  package body Nude is separate;

  package Dimd is

    use SI;

    function Schottky_Langmuir     (Volt: Voltage; Dist: Length) return Current_Density;
    function Schottky_Langmuir_Opt (Volt: Voltage; Dist: Length) return Current_Density;

    procedure Matrix_Rotate;
    procedure Quaternion_Rotate;

  end Dimd;
  package body Dimd is separate;

  type Repetitions is range 1 .. 1_000_000;

  Nude_Start, Nude_End,
  Opti_Start, Opti_End,
  Strl_Start, Strl_End,
  Drsd_Start, Drsd_End: Time;

  Quat_Start, Quat_End,
  Matr_Start, Matr_End: Time;

  function Safety return String is  -- Checked or Unchecked variant?
    use SI;
    X: Length;
  begin
    X := 1.0*"s";
    return " Unchecked";
  exception
    when others =>
      return "  Checked ";
  end Safety;

begin

  -- Schottky Langmuir

  declare
    J: Float;
  begin
    Nude_Start := Clock;
    for R in Repetitions loop
      -- Volt: constant Voltage := 50.0 * "daV";
      -- Dist: constant Length  := 20.0 * "cm";
      J := Nude.Schottky_Langmuir (50.0E+1 * (1.0 + Float (R) * 1.0E-6),
                                   20.0E-2 * (1.0 - Float (R) * 1.0E-6));
    end loop;
    Nude_End := Clock;
  end;

  declare
    use SI;
    Volt: constant Voltage := 50.0 * "daV";
    Dist: constant Length  := 20.0 * "cm";
    J   : Current_Density;
  begin
    Opti_Start := Clock;
    for R in Repetitions loop
      J := Dimd.Schottky_Langmuir_Opt (Volt * (1.0 + Float (R) * 1.0E-6),
                                       Dist * (1.0 - Float (R) * 1.0E-6));
    end loop;
    Opti_End := Clock;
  end;

  declare
    use SI;
    Volt: constant Voltage := 50.0 * "daV";
    Dist: constant Length  := 20.0 * "cm";
    J   : Current_Density;
  begin
    Strl_Start := Clock;
    for R in Repetitions loop
      J := Dimd.Schottky_Langmuir (Volt * (1.0 + Float (R) * 1.0E-6),
                                   Dist * (1.0 - Float (R) * 1.0E-6));
    end loop;
    Strl_End := Clock;
  end;

  declare
    use SI;
    J: Current_Density;
  begin
    Drsd_Start := Clock;
    for R in Repetitions loop
      J := Dimd.Schottky_Langmuir (50.0 * "daV" * (1.0 + Float (R) * 1.0E-6),
                                   20.0 * "cm"  * (1.0 - Float (R) * 1.0E-6));
    end loop;
    Drsd_End := Clock;
  end;

  -- Evaluation

  Put_Line (Safety);  New_Line;

  declare

    Delta_Nude: constant Duration := Nude_End - Nude_Start;
    Delta_Opti: constant Duration := Opti_End - Opti_Start;
    Delta_Strl: constant Duration := Strl_End - Strl_Start;
    Delta_Drsd: constant Duration := Drsd_End - Drsd_Start;

  begin

    Put_Line ("Schottky_Langmuir");
    Put ("   Nude   ");  Put (Delta_Nude);  Put (         1.0         , Aft => 1);  New_Line;
    Put ("   Opti   ");  Put (Delta_Opti);  Put (Delta_Opti/Delta_Nude, Aft => 1);  Put (         1.0         , Aft => 1);  New_Line;
    Put ("   Strl   ");  Put (Delta_Strl);  Put (Delta_Strl/Delta_Nude, Aft => 1);  Put (Delta_Strl/Delta_Opti, Aft => 1);  Put (         1.0         , Aft => 1);  New_Line;
    Put ("   Drsd   ");  Put (Delta_Drsd);  Put (Delta_Drsd/Delta_Nude, Aft => 1);  Put (Delta_Drsd/Delta_Opti, Aft => 1);  Put (Delta_Drsd/Delta_Strl, Aft => 1);  New_Line;
    New_Line;

  end;

  -- Matrix and Quaternions

  Matr_Start := Clock;
  for I in Repetitions loop
    Dimd.Matrix_Rotate;
  end loop;
  Matr_End := Clock;


  Quat_Start := Clock;
  for I in Repetitions loop
    Dimd.Quaternion_Rotate;
  end loop;
  Quat_End := Clock;

  declare

    Delta_Mat: constant Duration := Matr_End - Matr_Start;
    Delta_Qut: constant Duration := Quat_End - Quat_Start;

  begin

    Put_Line ("Transformation");
    Put_Line ("                    -- Matrix -           Quaternion");
    Put (Safety);  Put (Delta_Mat);  Put (Delta_Qut);  New_Line;
    New_Line;
    Put ("    Matrix");  Put (Delta_Mat);  Put (        1.0        , Aft => 1);  New_Line;
    Put ("Quaternion");  Put (Delta_Qut);  Put (Delta_Qut/Delta_Mat, Aft => 1);  New_Line;

  end;

end Measure;
