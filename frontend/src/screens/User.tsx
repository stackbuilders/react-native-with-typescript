import React, { useEffect, useState } from "react";
import { ActivityIndicator, Text, View } from "react-native";
import { List } from "../user/components/List";
import { Users } from "../user/types";

interface IUserScreenState {
  loading: boolean;
  users: Users;
  error: string | null;
}

export const User: React.FC = () => {
  const [{ error, loading, users }, setState] = useState<IUserScreenState>({
    error: null,
    loading: true,
    users: []
  });
  useEffect(() => {
    fetch("http://fbd5c453.ngrok.io/users", {
      headers: {
        Accept: "application/json",
        "Content-Type": "application/json"
      },
      method: "GET"
    })
      .then(response => {
        if (response.ok) {
          response
            .json()
            .then(us => setState({ loading: false, users: us, error: null }));
        } else {
          setState({
            error: `Server responded with error ${response.status}.`,
            loading: false,
            users: []
          });
        }
      })
      .catch(err => {
        setState({
          error: err,
          loading: false,
          users: []
        });
      });
  }, [null]);
  if (loading) {
    return (
      <View style={{ flex: 1, justifyContent: "center" }}>
        <ActivityIndicator size="large" />
      </View>
    );
  }
  if (error) {
    return (
      <View style={{ flex: 1, justifyContent: "center" }}>
        <Text style={{ color: "red", textAlign: "center", fontSize: 20 }}>
          {error}
        </Text>
      </View>
    );
  }
  return (
    <View style={{ flex: 1 }}>
      <List users={users} />
    </View>
  );
};
