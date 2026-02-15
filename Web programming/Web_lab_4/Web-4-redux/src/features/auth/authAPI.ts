import { AuthCredentials } from "./authSlice";

export interface LoginRequest {
  username: string;
  password: string;
}

export interface LogoutRequest {
  token: string;
}

export interface AuthResponse {
  result: string;
  token: string;
}

export const login = async (credentials: AuthCredentials): Promise<AuthResponse> => {
  try {
    const response = await fetch("api/user/login", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        username: credentials.username,
        password: credentials.password,
      }),
    });

    if (!response.ok) {
      const errorData = await response.json();
      return {
        result: errorData.result || `HTTP error! status: ${response.status}`,
        token: "",
      };
    }

    const data = await response.json();
    return {
      result: data.result,
      token: data.token,
    };
  } catch (error) {
    console.error("Error during login:", error);
    return {
      result: error instanceof Error ? error.message : "Unknown error occurred",
      token: "",
    };
  }
};

export const logout = async (token: string): Promise<{ result?: string; success: boolean }> => {
  try {
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };
    headers["Authorization"] = `Bearer ${token}`;

    const response = await fetch("api/user/logout", {
      method: "GET",
      headers,
    });

    if (!response.ok) {
      const errorData = await response.json();
      return {
        result: errorData.result || `HTTP error! status: ${response.status}`,
        success: false,
      };
    }

    return { success: true };
  } catch (error) {
    console.error("Error during logout:", error);
    return {
      result: error instanceof Error ? error.message : "Unknown error occurred",
      success: false,
    };
  }
};
