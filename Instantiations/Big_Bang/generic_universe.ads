----------------------------------------------------------------------------
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

with Ada.Numerics.Generic_Elementary_Functions;
--with Ada.Numerics.Generic_Real_Arrays;
with Ada.Text_IO;

with Generic_SI.Generic_Text_IO;
with Generic_SI.Generic_Natural_Constants;
--with Generic_SI.Generic_Polynomials;
--with Generic_SI.Generic_Temperatures.Generic_Text_IO;
--with Generic_SI.Generic_Vector_Space.Generic_Transformation;
--with Generic_SI.Generic_Quaternion_Space.Generic_Transformation;

with Actual_Dimensions;

generic

  type Real is digits <>;

package Generic_Universe is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      3 May 2026
  --====================================================================
  -- Alternative instantiation.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  03.05.2026
  --====================================================================

  package Real_IO                    is new Ada.Text_IO.Float_IO (Real'Base);
  package Elementary_Functions       is new Ada.Numerics.Generic_Elementary_Functions (Real'Base);
--package Real_Arrays                is new Ada.Numerics.Generic_Real_Arrays (Real);
  package SI                         is new Generic_SI (Real, Elementary_Functions, Actual_Dimensions);
  package SI_IO                      is new SI.Generic_Text_IO (Real_IO);
  package Natural_Constants          is new SI.Generic_Natural_Constants;
--package Temperatures               is new SI.Generic_Temperatures;
--package Temperature_IO             is new Temperatures.Generic_Text_IO (Real_IO);
--package Polynomials                is new SI.Generic_Polynomials;
--package Vectors                    is new SI.Generic_Vector_Space (Real_Arrays);
--package Vector_Transformations     is new Vectors.Generic_Transformation;
--package Quaternions                is new SI.Generic_Quaternion_Space (Real_Arrays, Vectors);
--package Quaternion_Transformations is new Quaternions.Generic_Transformation;

end Generic_Universe;
