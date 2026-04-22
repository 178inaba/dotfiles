import type { Plugin } from "@opencode-ai/plugin"

export const NotificationPlugin: Plugin = async ({ $ }) => {
  return {
    "session.idle": async () => {
      await $`afplay /System/Library/Sounds/Purr.aiff`
    },
    "permission.asked": async () => {
      await $`afplay /System/Library/Sounds/Sosumi.aiff`
    },
  }
}
