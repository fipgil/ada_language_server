------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with VSS.Strings.Hash;

with LSP_Gen.Mappings;

package body LSP_Gen.Dependencies is
   use type VSS.Strings.Virtual_String;
   use all type LSP_Gen.Entities.Enum.AType_Variant;

   function Dependency
     (List : LSP_Gen.Entities.Property_Vector) return Dependency_Map;

   function Dependency
     (List : LSP_Gen.Entities.AType_Vector) return Dependency_Map;

   function Dependency (Item : LSP_Gen.Entities.AType) return Dependency_Map;

   function Dependency
     (Item : LSP_Gen.Entities.Property) return Dependency_Map;

   ------------
   -- Append --
   ------------

   procedure Append
     (Self  : in out Dependency_Map;
      Items : Dependency_Map'Class) is
   begin
      for J in Items.Iterate loop
         declare
            Key : constant LSP_Gen.Entities.AType :=
              Dependency_Maps.Key (J);

            Value : constant Dependency_Info := Dependency_Maps.Element (J);

            Ok     : Boolean;
            Ignore : Dependency_Maps.Cursor;
         begin
            Self.Insert (Key, Dependency_Maps.Element (J), Ignore, Ok);

            if not Ok then
               pragma Assert (Value.Full_Name = Self.Element (Key).Full_Name);

               Self (Key).Has_Option :=
                 Self.Element (Key).Has_Option or Value.Has_Option;
            end if;
         end;
      end loop;
   end Append;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Item : LSP_Gen.Entities.AType) return Dependency_Map is
   begin
      return Result : Dependency_Map do
         case Item.Union.Kind is
            when reference =>
               Result.Insert
                 (Item,
                  (LSP_Gen.Mappings.Short_Name (Item, ""),
                   LSP_Gen.Mappings.Full_Name (Item, ""),
                   False));

            when an_array =>
               Result.Append (Dependency (Item.Union.an_array.element.Value));

            when map =>
               Result.Append (Dependency (Item.Union.map.value.Value));

            when tuple =>
               Result.Append (Dependency (Item.Union.tuple.items));

            when literal =>
               Result.Append
                 (Dependency
                    (Item.Union.literal.value.properties));

            when a_or =>
               declare
                  use all type LSP_Gen.Mappings.Or_Mapping_Kind;

                  Map : constant LSP_Gen.Mappings.Or_Mapping :=
                    LSP_Gen.Mappings.Get_Or_Mapping (Item.Union.a_or.items);
               begin
                  Result.Append (Dependency (Item.Union.a_or.items));

                  if Map.Kind = Two_Literals then
                     declare
                        List : LSP_Gen.Entities.Property_Vector renames
                          Map.Second.Union.literal.value.properties;
                     begin
                        for J in 1 .. List.Length loop
                           if Result.Contains (List (J).a_type) then
                              Result (List (J).a_type).Has_Option := True;
                           end if;
                        end loop;
                     end;
                  end if;
               end;

            when others =>
               null;
         end case;
      end return;
   end Dependency;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (Item : LSP_Gen.Entities.Property) return Dependency_Map
   is
   begin
      return Result : Dependency_Map := Dependency (Item.a_type) do
         if Item.optional and then Result.Contains (Item.a_type) then
            Result (Item.a_type).Has_Option := True;
         end if;
      end return;
   end Dependency;

   ----------------
   -- Dependency --
   ----------------

   function Dependency (List : LSP_Gen.Entities.AType_Vector)
     return Dependency_Map
   is
   begin
      return Result : Dependency_Map do
         for J in 1 .. List.Length loop
            Result.Append (Dependency (List (J)));
         end loop;
      end return;
   end Dependency;

   ----------------
   -- Dependency --
   ----------------

   function Dependency
     (List : LSP_Gen.Entities.Property_Vector) return Dependency_Map is
   begin
      return Result : Dependency_Map do
         for J in 1 .. List.Length loop
            Result.Append (Dependency (List (J)));
         end loop;
      end return;
   end Dependency;

   ----------
   -- Hash --
   ----------

   function Hash (Self : LSP_Gen.Entities.AType)
     return Ada.Containers.Hash_Type
   is
      use type Ada.Containers.Hash_Type;

      function Hash (List : LSP_Gen.Entities.AType_Vector)
        return Ada.Containers.Hash_Type;

      function Hash (List : LSP_Gen.Entities.Property_Vector)
        return Ada.Containers.Hash_Type;

      ----------
      -- Hash --
      ----------

      function Hash (List : LSP_Gen.Entities.AType_Vector)
        return Ada.Containers.Hash_Type
      is
         Result : Ada.Containers.Hash_Type := 0;
      begin
         for J in 1 .. List.Length loop
            Result := Result * 101 + Hash (List (J));
         end loop;

         return Result;
      end Hash;

      ----------
      -- Hash --
      ----------

      function Hash (List : LSP_Gen.Entities.Property_Vector)
        return Ada.Containers.Hash_Type
      is
         Result : Ada.Containers.Hash_Type := 0;
      begin
         for J in 1 .. List.Length loop
            Result := Result * 101 + Hash (List (J).a_type) +
              VSS.Strings.Hash (List (J).name);
         end loop;

         return Result;
      end Hash;

      Result : Ada.Containers.Hash_Type :=
        LSP_Gen.Entities.Enum.AType_Variant'Pos (Self.Union.Kind);
   begin
      case Self.Union.Kind is
         when base =>
            Result := Result * 101 +
              LSP_Gen.Entities.Enum.BaseTypes'Pos (Self.Union.base.name);

         when reference =>
            Result := Result * 101 +
              VSS.Strings.Hash (Self.Union.reference.name);

         when stringLiteral =>
            Result := Result * 101 +
              VSS.Strings.Hash (Self.Union.stringLiteral.value);

         when integerLiteral
            | booleanLiteral =>

            raise Program_Error;

         when an_array =>
            Result := Result * 101 +
              Hash (Self.Union.an_array.element.Value);

         when map =>
            Result := Result * 101 +
              Hash (Self.Union.map.value.Value);

         when an_and =>
            raise Program_Error;
         when a_or =>
            Result := Result * 101 + Hash (Self.Union.a_or.items);
         when tuple =>
            Result := Result * 101 + Hash (Self.Union.tuple.items);
         when literal =>
            Result := Result * 101 +
              Hash (Self.Union.literal.value.properties);
      end case;

      return Result;
   end Hash;

   ----------------
   -- References --
   ----------------

   function References
     (Item : LSP_Gen.Entities.Structure) return Dependency_Map
   is
   begin
      return Result : Dependency_Map do
         if Item.name = "LSPObject" then
            Result.Insert
              ((Union => (reference, (name => "LSPAny"))),
               ("LSPAny", "LSP.Structurs.LSPAny", False));
         else
            Result.Append (Dependency (Item.extends));
            Result.Append (Dependency (Item.mixins));
            Result.Append (Dependency (Item.properties));
         end if;
      end return;
   end References;

   ----------------
   -- References --
   ----------------

   function References (Item : LSP_Gen.Entities.TypeAlias)
                        return Dependency_Map is
   begin
      if Item.name = "LSPAny" then
         return Empty;
      else
         return Dependency (Item.a_type);
      end if;
   end References;

end LSP_Gen.Dependencies;
