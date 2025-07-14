------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2025 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
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
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

package body Generic_SI.Generic_Temperatures.Generic_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      3 July 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.10.2002
  --  C.G.    1.1  15.11.2002 Use Real'Base
  --  C.G.    1.2  10.03.2003 IO_Customization
  --  C.G.    2.0  01.07.2025 Resurrect unconstrained_checked_si-
  --                          generic_temperatures-generic_text_io and
  --                          rename; overhaul
  --  C.G.    2.1  03.07.2025 Get, Put for Strings
  --====================================================================

  use Real_Text_IO;

  procedure Get (X    : out Celsius;
                 Width: in  Field := 0) is
  begin
    Get (Current_Input, X, Width);
  end Get;

  procedure Get (File : in  File_Type;
                 X    : out Celsius;
                 Width: in  Field := 0) is
    Unit : String (1 .. 2);
    C    : Character;
    EoL  : Boolean;
    Count: Natural := 0;
  begin
    Get (File, X.Value, Width);
    -- Instead of this loop, we could just check whether "°C" follows.
    -- In order to be compatible with Get for Item , this is not done.
    loop
      Look_Ahead (File, C, EoL);
      exit when EoL or else C = ' ';
      Count := Count + 1;
      Get (File, Unit (Integer'Min (Unit'Last, Count)));
    end loop;
    if Count = 2 and Unit = "°C" then
      return;
    end if;
    raise Unit_Error;
  end Get;

  procedure Put (X   : in Celsius;
                 Fore: in Field := Default_Fore;
                 Aft : in Field := Default_Aft;
                 Exp : in Field := Default_Exp) is
  begin
    Put (Current_Output, X, Fore, Aft, Exp);
  end Put;

  procedure Put (File: in File_Type;
                 X   : in Celsius;
                 Fore: in Field := Default_Fore;
                 Aft : in Field := Default_Aft;
                 Exp : in Field := Default_Exp) is
  begin
    Put (File, X.Value, Fore, Aft, Exp);
    Put (File, "°C");
  end Put;

  procedure Get (From: in  String;
                 X   : out Celsius;
                 Last: out Positive) is
    Count: Natural := 0;
  begin
    Get (From, X.Value, Last);
    for I in Last + 1 .. From'Last loop
      exit when From (I) = ' ';
      Last  := I;
      Count := Count + 1;
    end loop;
    if Count = 2 and From (Last - Count + 1 .. Last) = "°C" then
      return;
    end if;
    raise Unit_Error;
  end Get;

  procedure Put (To  : out String;
                 X   : in  Celsius;
                 Aft : in  Field  := Default_Aft;
                 Exp : in  Field  := Default_Exp) is
  begin
    Put (To (To'First .. To'Last - 2), X.Value, Aft, Exp);
    To (To'Last - 1 .. To'Last) := "°C";
  end Put;

end Generic_SI.Generic_Temperatures.Generic_Text_IO;
