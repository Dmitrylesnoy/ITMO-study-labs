import { Routes, Route, Navigate } from "react-router-dom";
import { MainPage } from "./features/mainPage/MainPage";
import { LoginPage } from "./features/LoginPage/LoginPage";
import "./App.css";
import logo from "./logo.svg";
import { useSelector } from "react-redux";
import { RootState } from "./app/store";

function App() {
  const { isAuthenticated, loading } = useSelector((state: RootState) => state.auth);

  if (loading && isAuthenticated) {
    return (
      <div className="loading">
        <img src={logo} className="logo" alt="Logo" />
      </div>
    );
  }

  return (
    <Routes>
      <Route path="/login" element={<LoginPage />} />
      <Route path="/main" element={<MainPage />} />
      <Route path="/*" element={isAuthenticated ? <Navigate to="/main" replace /> : <Navigate to="/login" replace />} />
    </Routes>
  );
}

export default App;
