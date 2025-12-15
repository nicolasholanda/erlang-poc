"use client"

import type { FC } from "react"

interface SidebarProps {
  users: string[]
  currentUsername: string
  isOpen: boolean
  onClose: () => void
}

export const Sidebar: FC<SidebarProps> = ({ users, currentUsername, isOpen, onClose }) => {
  return (
    <div className={`sidebar ${isOpen ? "open" : ""}`} role="complementary" aria-label="User list sidebar">
      <div className="sidebar-header">
        <h2>Group Chat</h2>
        <button className="close-btn" onClick={onClose} aria-label="Close sidebar">
          ×
        </button>
      </div>
      <div className="users-section">
        <h3>Online Users ({users.length})</h3>
        <ul className="user-list" role="list">
          {users.map((user) => (
            <li key={user} className={user === currentUsername ? "current-user" : ""} role="listitem">
              <span className="user-status"></span>
              {user}
              {user === currentUsername && " (you)"}
            </li>
          ))}
        </ul>
      </div>
    </div>
  )
}
