import { createAppContainer, createStackNavigator } from "react-navigation";
import { User } from "./screens/User";

export const AppNavigator = createAppContainer(
  createStackNavigator(
    {
      Users: {
        navigationOptions: {
          title: "Quito Lambda - Users"
        },
        screen: User
      }
    },
    { initialRouteName: "Users" }
  )
);
