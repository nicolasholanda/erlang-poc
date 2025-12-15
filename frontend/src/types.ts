export interface Message {
  from: string
  text: string
  ts: number
}

export interface WebSocketState {
  connected: boolean
  users: string[]
  messages: Message[]
  typingUsers: string[]
}

export type ConnectionStatus = "connected" | "connecting" | "disconnected"
