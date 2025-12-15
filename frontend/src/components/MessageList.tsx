"use client"

import { type FC, useEffect, useRef } from "react"
import type { Message } from "../types"

interface MessageListProps {
  messages: Message[]
  currentUsername: string
  typingUsers: string[]
}

export const MessageList: FC<MessageListProps> = ({ messages, currentUsername, typingUsers }) => {
  const messagesEndRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: "smooth" })
  }, [messages])

  const formatTime = (ts: number) => {
    const date = new Date(ts)
    return date.toLocaleTimeString("en-US", { hour: "2-digit", minute: "2-digit" })
  }

  return (
    <div className="message-list" role="log" aria-live="polite" aria-label="Chat messages">
      {messages.map((msg, idx) => {
        const isOwn = msg.from === currentUsername
        return (
          <div
            key={idx}
            className={`message ${isOwn ? "own" : "other"}`}
            role="article"
            aria-label={`Message from ${msg.from}`}
          >
            <div className="message-bubble">
              {!isOwn && <div className="message-sender">{msg.from}</div>}
              <div className="message-text">{msg.text}</div>
              <div className="message-time">{formatTime(msg.ts)}</div>
            </div>
          </div>
        )
      })}
      {typingUsers.length > 0 && (
        <div className="typing-indicator" aria-live="polite">
          {typingUsers.join(", ")} {typingUsers.length === 1 ? "is" : "are"} typing...
        </div>
      )}
      <div ref={messagesEndRef} />
    </div>
  )
}
