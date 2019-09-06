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

export type User = INormalUser | IAdminUser
export type Users = User[];

type Omit<T, K extends keyof T> = Pick<T, Exclude<keyof T, K>>;

export type PostUser = Omit<IApiUser, "id">;

export type UpdateUser = Omit<PostUser, "name" | "type">;
