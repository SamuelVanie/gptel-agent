---
name: managed-discussions
description: Create and manage separate, persistent gptel conversations.
compatibility: Requires gptel-agent with the discussion tools installed.
allowed-tools: Discussion DiscussionControl
---

Loading this skill enables the already registered `Discussion` and
`DiscussionControl` tools for this conversation, including the current turn.
Child conversations load their own skills independently.

Use `Discussion` when the user wants a separate, persistent conversation they
can also interact with. Use `Agent` for a bounded worker whose findings belong
in the current task. Discussions run independently and can outlive your turn.

Create a discussion with a clear title and self-contained opening prompt.
It starts without your transcript or attached context. Use an existing `preset`
or a working `directory` when the conversation needs different settings.

Use `DiscussionControl` to list or read owned discussions, send a follow-up or
steering message, or close a discussion while retaining its transcript. Do useful
work while a discussion runs; wait for events when its result is needed and no
independent work remains. Completion and attention reports arrive automatically;
treat them as evidence to assess.

For a pending question, answer with its `request_id` when the answer is known,
or ask the user. For a tool approval, use `request_approval` to present the actual
calls to the user, or `reject` when they should not proceed. Do not infer
approval from silence. Permission to manage a discussion does not approve its
tool calls. The user may respond or intervene
directly in the child buffer; refresh its state if a request is stale.
