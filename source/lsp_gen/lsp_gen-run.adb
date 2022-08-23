------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                     Copyright (C) 2018-2022, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Streams.Stream_IO;

with VSS.Application;
with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;

with LSP_Gen.Entities;
with LSP_Gen.Entities.Inputs;
with LSP_Gen.Enumerations;
with LSP_Gen.Structures;
with LSP_Gen.Outputs;

procedure LSP_Gen.Run is
   Arg   : constant VSS.Strings.Virtual_String :=
     VSS.Application.Arguments.Element (1);
   File  : Ada.Streams.Stream_IO.File_Type;
   Raw   : VSS.Stream_Element_Vectors.Stream_Element_Vector;
   Input : aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

   Reader : VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
   MetaModel : LSP_Gen.Entities.MetaModel;
   Success : Boolean := True;
begin
   Ada.Streams.Stream_IO.Open
     (File,
      Ada.Streams.Stream_IO.In_File,
      VSS.Strings.Conversions.To_UTF_8_String (Arg));

   while not Ada.Streams.Stream_IO.End_Of_File (File) loop
      declare
         Data : Ada.Streams.Stream_Element_Array (1 .. 256);
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Ada.Streams.Stream_IO.Read (File, Data, Last);
         for X of Data (1 .. Last) loop
            Raw.Append (X);
         end loop;
      end;
   end loop;

   Input.Set_Data (Raw);
   Reader.Set_Stream (Input'Unchecked_Access);
   Reader.Read_Next;
   pragma Assert (Reader.Is_Start_Document);
   Reader.Read_Next;
   LSP_Gen.Entities.Inputs.Input_MetaModel (Reader, MetaModel, Success);
   pragma Assert (Success);
   LSP_Gen.Enumerations.Write_Types (MetaModel.enumerations);
   LSP_Gen.Structures.Write_Types (MetaModel);
   LSP_Gen.Outputs.Write (MetaModel);
end LSP_Gen.Run;
