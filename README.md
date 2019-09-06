# Quito Lambda - React Native + Typescript

## Steps
- Install `react-native-cli`:
``` bash
npm install -g react-native-cli
```
- Create a new project with typescript template
```bash
react-native init [project name] --template typescript
```
- Create new environment variable `ANDROID_SDK_ROOT` with the path of your `~/Android/Sdk/.
- Create android and ios scripts on `package.json`
```json
  "android": "react-native run-android"
  "ios": "react-native run-ios"
```
- Execute `npm run android` or `yarn android` to see your initial application.
- Remove `.prettierrc.js`.
- Add a tslinter to the project.
```bash
npm install --save-dev tslint tslint-react tslint-config-prettier
yarn install --dev tslint tslint-react tslint-config-prettier
```
- Create `tslint.json` file.
```json
{
  "extends": ["tslint:recommended", "tslint-react", "tslint-config-prettier"]
}
```
- Add rule `"resolveJsonModule": true` to `tsconfig` and change `index.js` to `index.tsx`.
```json
{
  "resolveJsonModule": true
}
```
- Add type definitions for `User`.
``` typescript
export type UserType = "admin" | "normal";

export type Hobby = "soccer" | "basketball" | "baseball";

export interface IApiUser {
  id: number;
  name: string;
  age: number;
  hobbies: Hobby[];
  type: UserType;
}

export interface INormalUser extends IApiUser {
  type: "normal";
}

export interface IAdminUser extends IApiUser {
  type: "admin";
}

export type Users = IApiUser[];

type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;

export type PostUser = Omit<IApiUser, "id">;

export type UpdateUser = Omit<PostUser, "name" | "type">;
```
- Create components for normal and admin user.
``` javascript
interface INormalProps {
  user: INormalUser;
}

export const Normal: React.FC<INormalProps> = ({ user: { name, hobbies } }) => (
  <View style={{ flex: 1, flexDirection: "row" }}>
    <Text style={{ fontWeight: "bold" }}>{name}</Text>
    <Text>Hobbies</Text>
    {hobbies.map((hobby, index) => (
      <Text key={`normal-user-${index}`}>{hobby}</Text>
    ))}
  </View>
);

interface IAdminProps {
  user: IAdminUser;
}

export const Admin: React.FC<IAdminProps> = ({ user: { name, age } }) => (
  <View style={{ flex: 1, flexDirection: "row" }}>
    <Text style={{ fontWeight: "bold" }}>Welcome {name}</Text>
    <Text>You can do anything with {age} years!</Text>
  </View>
);
```
- Add a component to display a list of users
``` javascript
interface IUserListProps {
  users: Users;
}

export const List: React.FC<IUserListProps> = ({ users }) => {
  const renderUser: ListRenderItem<User> = ({ item, index }) => {
    if (item.type === "admin") {
      return <Admin user={item} />;
    }
    return <Normal user={item} />;
  };
  const keyExtractor = (item: User, index: number) => `${item.type}-${index}`;
  return (
    <FlatList
      keyExtractor={keyExtractor}
      data={users}
      renderItem={renderUser}
    />
  );
};
```
- Create screen to load user data from API
``` javascript

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
  });
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
        <Text style={{ color: "red" }}>{error}</Text>
      </View>
    );
  }
  return (
    <View style={{ flex: 1 }}>
      <List users={users} />
    </View>
  );
};
```
- Add navigation libraries
``` bash
yarn add react-native-gesture-handler react-native-reanimated react-navigation
npm install react-native-gesture-handler react-native-reanimated react-navigation
```
- Create navigation for the application
``` javascript
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
```
- Add navigation into entry point of the application
``` javascript
const App: React.FC = () => (
  <View style={{ flex: 1 }}>
    <AppNavigator />
  </View>
);
```
