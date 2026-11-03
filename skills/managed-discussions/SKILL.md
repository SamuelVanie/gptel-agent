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
work while a discussion runs. Ordinary replies and attention alerts stay in the
child; completing a turn never automatically reports its answer to you. Each
child has `DiscussionReport` and decides when to send you a useful update.
When you need an answer or progress update, explicitly ask it to report back in
your opening prompt or a `send` message. Use `read` for status, transcript,
failures, questions, approvals and pending reviews without waking the child.
Use `wait` when an explicit report is expected and no independent work remains;
silent completion does not produce an event, so waiting can time out.
Treat reports as evidence to assess. Avoid acknowledgements that create
unnecessary reply loops.

If you are a managed child, use `DiscussionReport` to answer a request from your
managing conversation, or share a finding or blocker that matters to its work.
Choose what to share; do not routinely report direct user exchanges or every
completed turn. Your ordinary answers remain in your own discussion. Only an
explicit report can wake the managing conversation.

For a pending question, answer with its `request_id` when the answer is known,
or ask the user. For a tool approval, use `request_approval` to present the actual
calls to the user, or `reject` when they should not proceed. Do not infer
approval from silence. Permission to manage a discussion does not approve its
tool calls. The user may respond or intervene
directly in the child buffer; refresh its state if a request is stale.
