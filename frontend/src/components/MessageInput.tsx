"use client"

import { type FC, useState, type KeyboardEvent, type ChangeEvent, useRef } from "react"

interface MessageInputProps {
  onSendMessage: (text: string) => void
  onTyping: (typing: boolean) => void
  disabled: boolean
}

export const MessageInput: FC<MessageInputProps> = ({ onSendMessage, onTyping, disabled }) => {
  const [text, setText] = useState("")
  const typingTimeoutRef = useRef<number | null>(null)

  const handleSend = () => {
    if (text.trim() && !disabled) {
      onSendMessage(text.trim())
      setText("")
      onTyping(false)
      if (typingTimeoutRef.current) {
        clearTimeout(typingTimeoutRef.current)
        typingTimeoutRef.current = null
      }
    }
  }

  const handleKeyDown = (e: KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault()
      handleSend()
    }
  }

  const handleChange = (e: ChangeEvent<HTMLTextAreaElement>) => {
    setText(e.target.value)

    if (!disabled) {
      onTyping(true)

      if (typingTimeoutRef.current) {
        clearTimeout(typingTimeoutRef.current)
      }

      typingTimeoutRef.current = window.setTimeout(() => {
        onTyping(false)
        typingTimeoutRef.current = null
      }, 1000)
    }
  }

  return (
    <div className="message-input-container">
      <textarea
        className="message-input"
        value={text}
        onChange={handleChange}
        onKeyDown={handleKeyDown}
        placeholder="Type a message..."
        disabled={disabled}
        rows={1}
        aria-label="Message input"
      />
      <button
        className="send-button"
        onClick={handleSend}
        disabled={disabled || !text.trim()}
        aria-label="Send message"
      >
        Send
      </button>
    </div>
  )
}
