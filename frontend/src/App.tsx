"use client"

import { useState, useEffect } from "react"
import { useWebsocket } from "./hooks/useWebsocket"
import { Sidebar } from "./components/Sidebar"
import { MessageList } from "./components/MessageList"
import { MessageInput } from "./components/MessageInput"

function App() {
  const [username, setUsername] = useState<string>("")
  const [usernameInput, setUsernameInput] = useState("")
  const [sidebarOpen, setSidebarOpen] = useState(false)
  const wsUrl =
    (typeof import.meta !== "undefined" && import.meta.env?.VITE_WS_URL) ||
    process.env.NEXT_PUBLIC_WS_URL ||
    "ws://localhost:8080"

  const { connect, sendMessage, sendTyping, state, status } = useWebsocket(wsUrl)

  useEffect(() => {
    const stored = localStorage.getItem("chat_username")
    if (stored) {
      setUsername(stored)
      connect(stored)
    }
  }, [connect])

  const handleUsernameSubmit = () => {
    if (usernameInput.trim()) {
      const name = usernameInput.trim()
      setUsername(name)
      localStorage.setItem("chat_username", name)
      connect(name)
    }
  }

  if (!username) {
    return (
      <div className="username-prompt" role="dialog" aria-labelledby="username-title">
        <div className="username-modal">
          <h1 id="username-title">Enter your username</h1>
          <input
            type="text"
            value={usernameInput}
            onChange={(e) => setUsernameInput(e.target.value)}
            onKeyDown={(e) => e.key === "Enter" && handleUsernameSubmit()}
            placeholder="Username"
            autoFocus
            aria-label="Username input"
          />
          <button onClick={handleUsernameSubmit} disabled={!usernameInput.trim()}>
            Join Chat
          </button>
        </div>
      </div>
    )
  }

  return (
    <div className="app">
      <div className="connection-status" role="status" aria-live="polite">
        <span className={`status-indicator ${status}`}></span>
        {status === "connected" ? "Connected" : status === "connecting" ? "Connecting..." : "Disconnected"}
      </div>

      <button
        className="sidebar-toggle"
        onClick={() => setSidebarOpen(true)}
        aria-label="Open sidebar"
        aria-expanded={sidebarOpen}
      >
        ☰
      </button>

      <div className="chat-container">
        <Sidebar
          users={state.users}
          currentUsername={username}
          isOpen={sidebarOpen}
          onClose={() => setSidebarOpen(false)}
        />

        <main className="chat-main" role="main">
          <MessageList
            messages={state.messages}
            currentUsername={username}
            typingUsers={state.typingUsers.filter((u) => u !== username)}
          />

          <MessageInput onSendMessage={sendMessage} onTyping={sendTyping} disabled={!state.connected} />
        </main>
      </div>
    </div>
  )
}

export default App
