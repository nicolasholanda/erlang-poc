"use client"

import { useState, useEffect, useRef, useCallback } from "react"
import type { WebSocketState, ConnectionStatus } from "../types"

export const useWebsocket = (wsUrl: string) => {
  const [state, setState] = useState<WebSocketState>({
    connected: false,
    users: [],
    messages: [],
    typingUsers: [],
  })
  const [status, setStatus] = useState<ConnectionStatus>("disconnected")
  const wsRef = useRef<WebSocket | null>(null)
  const reconnectTimeoutRef = useRef<number | null>(null)
  const reconnectAttemptsRef = useRef(0)
  const usernameRef = useRef<string>("")
  const typingTimeoutRef = useRef<{ [key: string]: number }>({})

  const connect = useCallback(
    (username: string) => {
      usernameRef.current = username

      if (wsRef.current?.readyState === WebSocket.OPEN) {
        return
      }

      setStatus("connecting")

      try {
        const ws = new WebSocket(wsUrl)

        ws.onopen = () => {
          setStatus("connected")
          setState((prev) => ({ ...prev, connected: true }))
          reconnectAttemptsRef.current = 0
          ws.send(JSON.stringify({ type: "join", username }))
        }

        ws.onmessage = (event) => {
          try {
            const data = JSON.parse(event.data)

            switch (data.type) {
              case "message":
                setState((prev) => ({
                  ...prev,
                  messages: [...prev.messages, { from: data.from, text: data.text, ts: data.ts }],
                }))
                break
              case "user_list":
                setState((prev) => ({ ...prev, users: data.users }))
                break
              case "user_join":
                setState((prev) => ({
                  ...prev,
                  users: prev.users.includes(data.user) ? prev.users : [...prev.users, data.user],
                }))
                break
              case "user_leave":
                setState((prev) => ({
                  ...prev,
                  users: prev.users.filter((u) => u !== data.user),
                  typingUsers: prev.typingUsers.filter((u) => u !== data.user),
                }))
                break
              case "typing":
                if (data.typing) {
                  setState((prev) => ({
                    ...prev,
                    typingUsers: prev.typingUsers.includes(data.username)
                      ? prev.typingUsers
                      : [...prev.typingUsers, data.username],
                  }))

                  if (typingTimeoutRef.current[data.username]) {
                    clearTimeout(typingTimeoutRef.current[data.username])
                  }

                  typingTimeoutRef.current[data.username] = window.setTimeout(() => {
                    setState((prev) => ({
                      ...prev,
                      typingUsers: prev.typingUsers.filter((u) => u !== data.username),
                    }))
                  }, 3000)
                } else {
                  setState((prev) => ({
                    ...prev,
                    typingUsers: prev.typingUsers.filter((u) => u !== data.username),
                  }))
                }
                break
            }
          } catch (e) {
            console.error("Failed to parse message:", e)
          }
        }

        ws.onerror = () => {
          setStatus("disconnected")
          setState((prev) => ({ ...prev, connected: false }))
        }

        ws.onclose = () => {
          setStatus("disconnected")
          setState((prev) => ({ ...prev, connected: false }))

          const delay = Math.min(1000 * Math.pow(2, reconnectAttemptsRef.current), 30000)
          reconnectAttemptsRef.current++

          reconnectTimeoutRef.current = window.setTimeout(() => {
            if (usernameRef.current) {
              connect(usernameRef.current)
            }
          }, delay)
        }

        wsRef.current = ws
      } catch (e) {
        console.error("Failed to connect:", e)
        setStatus("disconnected")
      }
    },
    [wsUrl],
  )

  const sendMessage = useCallback((text: string) => {
    if (wsRef.current?.readyState === WebSocket.OPEN && usernameRef.current) {
      wsRef.current.send(
        JSON.stringify({
          type: "message",
          username: usernameRef.current,
          text,
        }),
      )
    }
  }, [])

  const sendTyping = useCallback((typing: boolean) => {
    if (wsRef.current?.readyState === WebSocket.OPEN && usernameRef.current) {
      wsRef.current.send(
        JSON.stringify({
          type: "typing",
          username: usernameRef.current,
          typing,
        }),
      )
    }
  }, [])

  useEffect(() => {
    return () => {
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current)
      }
      Object.values(typingTimeoutRef.current).forEach((timeout) => clearTimeout(timeout))
      if (wsRef.current) {
        wsRef.current.close()
      }
    }
  }, [])

  return { connect, sendMessage, sendTyping, state, status }
}
