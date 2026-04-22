import type { Plugin } from "@opencode-ai/plugin"

export const NotificationPlugin: Plugin = async ({ $ }) => {
  return {
    event: async ({ event }) => {
      if (event.type === "session.idle") {
        await $`afplay /System/Library/Sounds/Purr.aiff`
      }
      if (event.type === "permission.asked") {
        await $`afplay /System/Library/Sounds/Sosumi.aiff`
      }
    },
  }
}
