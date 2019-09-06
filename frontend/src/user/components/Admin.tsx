import React from "react";
import { Text, View } from "react-native";
import { IAdminUser } from "../types";

interface IAdminProps {
  user: IAdminUser;
}

export const Admin: React.FC<IAdminProps> = ({ user: { name, age } }) => (
  <View style={{ flex: 1, flexDirection: "column", padding: 10 }}>
    <Text style={{ fontWeight: "bold" }}>Welcome {name}</Text>
    <Text>You can do anything with {age} years!</Text>
  </View>
);
