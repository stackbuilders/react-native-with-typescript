import React from "react";
import { FlatList, ListRenderItem } from "react-native";
import { User, Users } from "../types";
import { Admin } from "./Admin";
import { Normal } from "./Normal";

interface IUserListProps {
  users: Users;
}

const renderUser: ListRenderItem<User> = ({ item, index }) => {
  if (item.type === "admin") {
    return <Admin user={item} />;
  }
  return <Normal user={item} />;
};

const keyExtractor = (item: User, index: number) => `${item.type}-${index}`;

export const List: React.FC<IUserListProps> = ({ users }) => {
  return (
    <FlatList
      keyExtractor={keyExtractor}
      data={users}
      renderItem={renderUser}
    />
  );
};
