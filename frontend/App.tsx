import React from "react";
import { View } from "react-native";
import { AppNavigator } from "./src/Navigation";

const App: React.FC = () => (
  <View style={{ flex: 1 }}>
    <AppNavigator />
  </View>
);

export default App;
