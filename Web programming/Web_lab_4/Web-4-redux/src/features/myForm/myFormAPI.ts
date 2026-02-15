import { PointData } from "../myTable/myTableSlice";

export interface FormData {
  x: number;
  y: number;
  r: number;
  graphFlag?: boolean;
}

export interface ServerResponse {
  data?: PointData[];
  error?: string;
}

export const submitFormData = async (formData: FormData, token?: string): Promise<ServerResponse> => {
  try {
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };

    if (token) {
      headers["Authorization"] = `Bearer ${token}`;
    }

    const response = await fetch("api/form", {
      method: "POST",
      headers,
      body: JSON.stringify(formData),
    });

    if (!response.ok) {
      const errorData = await response.json();
      return {
        error: (errorData.error || "") + `HTTP error! status: ${response.status}`,
      };
    }

    const data = await response.json();
    return { data: data };
  } catch (error) {
    console.error("Error submitting form:", error);
    return {
      error: error instanceof Error ? error.message : "Unknown error occurred",
    };
  }
};
