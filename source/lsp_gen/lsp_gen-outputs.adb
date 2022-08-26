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

with VSS.Strings;

with LSP_Gen.Structures;
with LSP_Gen.Puts; use LSP_Gen.Puts;

package body LSP_Gen.Outputs is

   use type VSS.Strings.Virtual_String;
   use all type LSP_Gen.Entities.Enum.AType_Variant;

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String);

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean);

   procedure Write_Type
     (Name : VSS.Strings.Virtual_String;
      Tipe : LSP_Gen.Entities.AType;
      Spec : Boolean);

   procedure Write_Call
     (Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String);

   -----------
   -- Write --
   -----------

   procedure Write (Model : LSP_Gen.Entities.MetaModel) is
   begin
      Put_Line ("with VSS.JSON.Content_Handlers;");
      Put_Line ("with LSP.Enumerations;");
      Put_Line ("with LSP.Structures;");
      New_Line;
      Put_Line ("package LSP.Outputs is");
      Put_Line ("   pragma Preelaborate;"); New_Line;

      for J in 1 .. Model.enumerations.Length loop
         Write_Enum (Model.enumerations (J), Spec => True);
      end loop;

      for J in 1 .. Model.structures.Length loop
         declare
            Item : constant LSP_Gen.Entities.Structure :=
              Model.structures (J);
         begin
            Write_Subprogram_Definition (Item.name, "LSP.Structures.");
            Put_Line (" is null;"); New_Line;
         end;
      end loop;

      Put_Line ("end LSP.Outputs;");

      Put_Line ("with Ada.Containers;");
      Put_Line ("with Interfaces;");
      Put_Line ("with LSP.Structures;");
      New_Line;

      Put_Line ("package body LSP.Outputs is"); New_Line;
      Put_Line ("use type Interfaces.Integer_64;"); New_Line;
      Put_Line ("use type Ada.Containers.Count_Type;"); New_Line;

      for J in 1 .. Model.typeAliases.Length loop
         declare
            Item : constant LSP_Gen.Entities.TypeAlias :=
              Model.typeAliases (J);
         begin
            Write_Type (Item.name, Item.a_type, Spec => True);
         end;
      end loop;

      for J in 1 .. Model.enumerations.Length loop
         Write_Enum (Model.enumerations (J), Spec => False);
      end loop;

      for J in 1 .. Model.typeAliases.Length loop
         declare
            Item : constant LSP_Gen.Entities.TypeAlias :=
              Model.typeAliases (J);
         begin
            Write_Type (Item.name, Item.a_type, Spec => False);
         end;
      end loop;

      Put_Line ("end LSP.Outputs;");
   end Write;

   ----------------
   -- Write_Call --
   ----------------

   procedure Write_Call
     (Tipe   : LSP_Gen.Entities.AType;
      Suffix : VSS.Strings.Virtual_String) is
   begin
      Put ("Write_");
      Put_Id (Tipe.Union.reference.name);
      Put (Suffix);
   end Write_Call;

   ----------------
   -- Write_Enum --
   ----------------

   procedure Write_Enum
     (Info : LSP_Gen.Entities.Enumeration;
      Spec : Boolean)
   is
      Name : constant VSS.Strings.Virtual_String := Info.name;
   begin
      Write_Subprogram_Definition (Name, "LSP.Enumerations.");

      if Spec then
         Put_Line (";");
      else
         Put_Line (" is");
         Put_Line ("begin");
         Put_Line ("case Value is");

         for J in 1 .. Info.values.Length loop
            Put ("when LSP.Enumerations.");
            Put_Id (Info.values (J).name);
            Put_Line (" =>");

            case Info.a_type.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put ("Handler.String_Value (""");
                  Put (Info.values (J).value.String);
                  Put_Line (""");");

               when others =>
                  Put ("Handler.Integer_Value (");
                  Put (Info.values (J).value.Integer);
                  Put_Line (");");

            end case;
         end loop;

         Put_Line ("end case;");
         Put ("end Write_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Enum;

   ---------------------------------
   -- Write_Subprogram_Sefinition --
   ---------------------------------

   procedure Write_Subprogram_Definition
     (Name   : VSS.Strings.Virtual_String;
      Prefix : VSS.Strings.Virtual_String) is
   begin
      Put ("procedure Write_");
      Put_Id (Name);
      Put (" (Handler : in out ");
      Put_Line ("VSS.JSON.Content_Handlers.JSON_Content_Handler'Class;");
      Put ("Value : ");
      Put (Prefix);
      Put_Id (Name);
      Put (")");
   end Write_Subprogram_Definition;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Name : VSS.Strings.Virtual_String;
      Tipe : LSP_Gen.Entities.AType;
      Spec : Boolean) is
   begin
      if Name = "LSPAny" or Name = "LSPArray" then
         return;  --  TBD
      end if;

      Write_Subprogram_Definition (Name, "LSP.Structures.");

      if Spec then
         Put_Line (";");
      elsif Tipe.Union.Kind = reference then
         Put_Line (" renames");
         Write_Call (Tipe, "");
         Put_Line (";");
      else
         Put_Line ("is");
         Put_Line ("begin");

         case Tipe.Union.Kind is
            when a_or =>
               declare
                  use all type LSP_Gen.Structures.Or_Mapping_Kind;

                  Map : constant LSP_Gen.Structures.Or_Mapping :=
                    LSP_Gen.Structures.Get_Or_Mapping
                      (Tipe.Union.a_or.items);
               begin
                  case Map.Kind is
                     when Type_Or_Array =>
                        Put_Line ("if Value.Length = 1 then");
                        Write_Call
                          (Map.Array_Type.Union.an_array.element.Value,
                           " (Handler, Value (1));");
                        New_Line;

                        Put_Line ("else");

                        Put_Line ("Handler.Start_Array;");
                        Put_Line ("for J in 1 .. Value.Last_Index loop");
                        Write_Call
                          (Map.Array_Type.Union.an_array.element.Value,
                           " (Handler, Value (J));");
                        New_Line;
                        Put_Line ("end loop;");
                        Put_Line ("Handler.End_Array;");

                        Put_Line ("end if;");
                     when Unknown_Mapping =>
                        Put_Line ("case Value.Kind is");
                        for J in 1 .. Tipe.Union.a_or.items.Length loop
                           Put ("   when LSP.Structures.");
                           Put_Id
                             (LSP_Gen.Structures.Get_Variant
                                (Tipe.Union.a_or.items (J), J));
                           Put_Line (" =>");

                           --  Write_Call
                           --    (Tipe.Union.a_or.items (J),
                           --     " (Handler, Value." &
                           --     LSP_Gen.Structures.Get_Variant
                           --       (Tipe.Union.a_or.items (J), J) &
                           --     ");");

                           Put_Line ("null;");
                        end loop;
                        Put_Line ("end case;");
                     when others =>
                        Put_Line ("null;");
                  end case;
               end;
            when others =>
               Put_Line ("null;");
         end case;
         Put ("end Write_");
         Put_Id (Name);
         Put_Line (";");
      end if;

      New_Line;
   end Write_Type;

end LSP_Gen.Outputs;
