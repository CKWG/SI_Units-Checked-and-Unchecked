------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
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

with Ada.Text_IO;
use  Ada.Text_IO;

with Big_Bang;
use  Big_Bang;

procedure Tables is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      19 May 2026
  --====================================================================
  -- Three examples for tables.
  -- Write the tables, then read and verify the contents.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  30.03.2026
  --  C.G.    1.1  13.04.2026 Naming change Earth_Mass -> Earth.Mass
  --  C.G.    1.2  03.05.2026 Use new instantiation Big_Bang
  --  C.G.    2.0  13.05.2026 Elaborated table new
  --  C.G.    2.1  19.05.2026 Alternative reading of elaborated table
  --====================================================================

  use Universe;
  use SI, SI_IO, Natural_Constants;

  package Earth is
    Mass                  : constant SI.Mass         :=     5.9722E+24*"kg";  -- https://en.wikipedia.org/wiki/Earth_mass
    Equatorial_Radius     : constant Length          :=  6378.1370    *"km";  -- https://en.wikipedia.org/wiki/Earth_radius
    Polar_Radius          : constant Length          :=  6356.7523    *"km";  -- according to WGS 84
    Arithmetic_Mean_Radius: constant Length          := (2.0*Equatorial_Radius + Polar_Radius)/3.0;  -- 6371.0088 km
    Acceleration          : constant SI.Acceleration := Gravity * Mass / Arithmetic_Mean_Radius**2;
  end Earth;

  package Simple_Table is
    -- Column 1 .. Start-1 => Name
    -- Column Start        => X in given format
    Procedure Set (Start: in  Positive_Count);
    procedure Put (Name : in  String; X: in  Item; Fore, Aft, Exp: in Field; Dim: in String := "");
    procedure Get (Name : out String; X: out Item);
  end Simple_Table;

  package body Simple_Table is separate;

  package Aligned_Table is
    -- Column 1 .. Start-1 => Name
    -- Column Start        => Width characers for X in given numeric format,
    --                        unit following aligned
    procedure Set (Start: in  Positive_Count; Width: in Field);
    procedure Put (Name : in  String; X: in  Item; Aft, Exp: in Field; Dim: in String := "");
    procedure Get (Name : out String; X: out Item);
  end Aligned_Table;

  package body Aligned_Table is separate;

  package Elaborated_Table is
    -- Column 1 .. Start-1 => Name
    -- Column Start        => Width characers for X in given numeric format,
    --                        Pad blanks, Unit characters for the unit left or
    --                        right aligned
    procedure Set (Start: in  Positive_Count; Width, Pad: in Field; Unit: in Field'Base);
    procedure Put (Name : in  String; X: in  Item; Aft, Exp: in Field; Dim: in String := "");
    procedure Get (Name : out String; X: out Item);
  end Elaborated_Table;

  package body Elaborated_Table is separate;

  Result: File_Type;

begin

  Create (Result, Out_File, "Tables.txt");
  Set_Output (Result);

  Put_Line ("Simple_Table");
  Put_Line ("============");
  Put_Line ("Earth Data");
  Simple_Table.Set (Start => 24);
  Simple_Table.Put ("Mass"                  , Earth.Mass                  , Fore => 0, Aft => 4, Exp => 1);
  Simple_Table.Put (""                      , Earth.Mass                  , Fore => 0, Aft => 4, Exp => 1, Dim => "Rg"    );
  Simple_Table.Put ("Equatorial Radius"     , Earth.Equatorial_Radius     , Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Polar Radius"          , Earth.Polar_Radius          , Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Mean Radius"           , Earth.Arithmetic_Mean_Radius, Fore => 0, Aft => 4, Exp => 0, Dim => "km"    );
  Simple_Table.Put ("Gravitational Constant", Gravity                     , Fore => 0, Aft => 5, Exp => 2);
  Simple_Table.Put ("Acceleration"          , Earth.Acceleration          , Fore => 0, Aft => 4, Exp => 0, Dim => "m/s**2");
  Put_Line ("123456789012345678901234");

  New_Line;

  Put_Line ("Aligned_Table");
  Put_Line ("=============");
  Put_Line ("Earth Data");
  Aligned_Table.Set (Start => 24, Width => 12);
  Aligned_Table.Put ("Mass"                  , Earth.Mass                  , Aft => 4, Exp => 1);
  Aligned_Table.Put (""                      , Earth.Mass                  , Aft => 4, Exp => 0, Dim => "Rg");
  Aligned_Table.Put ("Equatorial Radius"     , Earth.Equatorial_Radius     , Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Polar Radius"          , Earth.Polar_Radius          , Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Mean Radius"           , Earth.Arithmetic_Mean_Radius, Aft => 4, Exp => 0, Dim => "km");
  Aligned_Table.Put ("Gravitational Constant", Gravity                     , Aft => 5, Exp => 2);
  Aligned_Table.Put ("Acceleration"          , Earth.Acceleration          , Aft => 4, Exp => 0, Dim => "m/s**2");
  Put_Line ("12345678901234567890123|23456789012*");

  New_Line;

  Put_Line ("Elaborated_Table");
  Put_Line ("================");
  Put_Line ("Earth Data");
  Elaborated_Table.Set (Start => 24, Width => 12, Pad => 3, Unit => 22);
  Elaborated_Table.Put ("Mass"                  , Earth.Mass                  , Aft => 4, Exp => 1);
  Elaborated_Table.Put (""                      , Earth.Mass                  , Aft => 4, Exp => 0, Dim => "Rg");
  Elaborated_Table.Put ("Equatorial Radius"     , Earth.Equatorial_Radius     , Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Polar Radius"          , Earth.Polar_Radius          , Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Mean Radius"           , Earth.Arithmetic_Mean_Radius, Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Gravitational Constant", Gravity                     , Aft => 5, Exp => 2);
  Elaborated_Table.Put ("Acceleration"          , Earth.Acceleration          , Aft => 4, Exp => 0, Dim => "m/s**2");
  Put_Line ("12345678901234567890123|23456789012---1234567890123456789012");

  New_Line;

  Put_Line ("Earth Data");
  Elaborated_Table.Set (Start => 24, Width => 12, Pad => 1, Unit => -21);
  Elaborated_Table.Put ("Mass"                  , Earth.Mass                  , Aft => 4, Exp => 1);
  Elaborated_Table.Put (""                      , Earth.Mass                  , Aft => 4, Exp => 0, Dim => "Rg");
  Elaborated_Table.Put ("Equatorial Radius"     , Earth.Equatorial_Radius     , Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Polar Radius"          , Earth.Polar_Radius          , Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Mean Radius"           , Earth.Arithmetic_Mean_Radius, Aft => 4, Exp => 0, Dim => "km");
  Elaborated_Table.Put ("Gravitational Constant", Gravity                     , Aft => 5, Exp => 2);
  Elaborated_Table.Put ("Acceleration"          , Earth.Acceleration          , Aft => 4, Exp => 0, Dim => "m/s**2");
  Put_Line ("12345678901234567890123|23456789012-123456789012345678901");

  declare
    Earth_Mass            : Mass;
    Equatorial_Radius     : Length;
    Polar_Radius          : Length;
    Arithmetic_Mean_Radius: Length;
    Gravity               : Item;
    Earth_Acceleration    : Acceleration;
    Name: String (101 .. 123);
    Last: Natural;
  begin
    Set_Output (Standard_Output);
    Reset (Result, In_File);
    Set_Input (Result);
    ---------------
    -- Simple Table
    ---------------
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    Skip_Line;
    --
    Simple_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Simple_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Simple_Table.Get (Name, Equatorial_Radius);
    Put_Line (Name & Boolean'Image (Equatorial_Radius = Earth.Equatorial_Radius));
    --
    Simple_Table.Get (Name, Polar_Radius);
    Put_Line (Name & Boolean'Image (Polar_Radius = Earth.Polar_Radius));
    --
    Simple_Table.Get (Name, Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image (Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    --
    Simple_Table.Get (Name, Gravity);
    Put_Line (Name & Boolean'Image (Gravity = Natural_Constants.Gravity));
    --
    Simple_Table.Get (Name, Earth_Acceleration);
    Put (Name & Boolean'Image (Earth_Acceleration = Earth.Acceleration));
    Put (Earth_Acceleration - Earth.Acceleration);
    New_Line (2);
    Skip_Line (2);
    ----------------
    -- Aligned Table
    ----------------
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    Skip_Line;
    --
    Aligned_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Aligned_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Aligned_Table.Get (Name, Equatorial_Radius);
    Put_Line (Name & Boolean'Image (Equatorial_Radius = Earth.Equatorial_Radius));
    --
    Aligned_Table.Get (Name, Polar_Radius);
    Put_Line (Name & Boolean'Image (Polar_Radius = Earth.Polar_Radius));
    --
    Aligned_Table.Get (Name, Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image (Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    --
    Aligned_Table.Get (Name, Gravity);
    Put_Line (Name & Boolean'Image (Gravity = Natural_Constants.Gravity));
    --
    Aligned_Table.Get (Name, Earth_Acceleration);
    Put (Name & Boolean'Image (Earth_Acceleration = Earth.Acceleration));
    Put (Earth_Acceleration - Earth.Acceleration);
    New_Line (2);
    Skip_Line (2);
    ---------------------------------
    -- Elaborated Table right-aligned
    ---------------------------------
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    Get_Line (Name, Last);
    Put_Line (Name (Name'First .. Last));
    -- Unit sign irrelevant for reading
    Elaborated_Table.Set (Start => 24, Width => 12, Pad => 3, Unit => -22);
    Skip_Line;
    --
    Elaborated_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Elaborated_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Elaborated_Table.Get (Name, Equatorial_Radius);
    Put_Line (Name & Boolean'Image (Equatorial_Radius = Earth.Equatorial_Radius));
    --
    Elaborated_Table.Get (Name, Polar_Radius);
    Put_Line (Name & Boolean'Image (Polar_Radius = Earth.Polar_Radius));
    --
    Elaborated_Table.Get (Name, Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image (Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    --
    Elaborated_Table.Get (Name, Gravity);
    Put_Line (Name & Boolean'Image (Gravity = Natural_Constants.Gravity));
    --
    Elaborated_Table.Get (Name, Earth_Acceleration);
    Put (Name & Boolean'Image (Earth_Acceleration = Earth.Acceleration));
    Put (Earth_Acceleration - Earth.Acceleration);
    New_Line (2);
    Skip_Line (2);
    --------------------------------
    -- Elaborated Table left-aligned
    --------------------------------
    Skip_Line;
    -- For reading, you can conflate the modifiers, just count spaces
    Elaborated_Table.Set (Start => 23, Width => 13, Pad => 0, Unit => 22);
    --
    Elaborated_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Elaborated_Table.Get (Name, Earth_Mass);
    Put_Line (Name & Boolean'Image (Earth_Mass = Earth.Mass));
    --
    Elaborated_Table.Get (Name, Equatorial_Radius);
    Put_Line (Name & Boolean'Image (Equatorial_Radius = Earth.Equatorial_Radius));
    --
    Elaborated_Table.Get (Name, Polar_Radius);
    Put_Line (Name & Boolean'Image (Polar_Radius = Earth.Polar_Radius));
    --
    Elaborated_Table.Get (Name, Arithmetic_Mean_Radius);
    Put (Name & Boolean'Image (Arithmetic_Mean_Radius = Earth.Arithmetic_Mean_Radius));
    Put (Arithmetic_Mean_Radius - Earth.Arithmetic_Mean_Radius);
    New_Line;
    --
    Elaborated_Table.Get (Name, Gravity);
    Put_Line (Name & Boolean'Image (Gravity = Natural_Constants.Gravity));
    --
    Elaborated_Table.Get (Name, Earth_Acceleration);
    Put (Name & Boolean'Image (Earth_Acceleration = Earth.Acceleration));
    Put (Earth_Acceleration - Earth.Acceleration);
    New_Line;
    --
    Close (Result);
  end;

end Tables;
