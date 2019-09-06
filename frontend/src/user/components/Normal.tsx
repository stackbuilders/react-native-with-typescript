import React from "react";
import { Text, View } from "react-native";
import { INormalUser } from "../types";

interface INormalProps {
  user: INormalUser;
}

export const Normal: React.FC<INormalProps> = ({ user: { name, hobbies } }) => (
  <View style={{ flex: 1, flexDirection: "column", padding: 10 }}>
    <Text style={{ fontWeight: "bold" }}>{name}</Text>
    <Text style={{ fontStyle: "italic", fontWeight: "bold" }}>Hobbies</Text>
    {hobbies.map((hobby, index) => (
      <Text key={`normal-user-${index}`}>- {hobby}</Text>
    ))}
  </View>
);
