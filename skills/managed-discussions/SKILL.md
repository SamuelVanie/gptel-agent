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
It starts without your transcript or attached context. Omit `preset` (or pass
an empty string) to inherit your current request settings, including local
changes. To use different settings, choose a name from the `Discussion` tool's
available gptel presets. These include all registered user-defined presets,
not just gptel-agent's presets or the `Agent` tool's allowed sub-agents.
The chosen preset overrides only the settings it specifies; other settings
remain inherited. For example, a planning conversation can create an
implementation discussion with `preset: "gptel-agent"` and the complete plan
in `prompt`. Your own preset remains unchanged. Use `directory` to select a
different working directory when needed.

Use `worktree: true` only when the user explicitly requests a Git worktree or
isolated Git workspace. Do not prefer worktrees by default or infer permission
from a general coding or delegation request. Otherwise omit `worktree` or pass
`false`. The source is `directory`, or
your current working directory when omitted. It must be in a Git working
repository with a commit. The child starts at the root of a randomly named
sibling worktree on a new branch at the source's HEAD; uncommitted and
untracked files are not copied. The result includes its path, branch, source
repository/directory and starting commit, also available through
`DiscussionControl list/read`. Closing a discussion retains its worktree and
branch for review, integration and later cleanup.

After the work is finished and integrated, use `DiscussionControl` with
`action: "cleanup"` and its `discussion_id` to remove the generated worktree.
Omit `delete_branch` (or use `false`) to keep the branch, or set
`delete_branch: true` to delete it too. Cleanup closes an idle discussion and
keeps its editable transcript. It also works after `close` or a startup failure.
It refuses running or queued work, other discussions still using the worktree,
live managed background processes, unsaved file edits and pending reviews.
Resolve these first; do not close active work just to bypass cleanup checks.
Dirty or locked worktrees, a switched branch and a detached HEAD are refused.
Git removal and branch deletion are never forced. Branch deletion uses
`git branch -d`: if it fails, the worktree is already removed and the branch is
retained. Inspect the reported cleanup states, integrate the branch, then retry
`cleanup` with `delete_branch: true`. Repeated successful calls are safe.

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
or ask the user. Tool approvals stay in the child discussion buffer, where only
the user can approve or reject them. An echo-area notice identifies the buffer
waiting for approval. Do not copy approval controls into the managing
conversation or try to resolve them through `DiscussionControl`. Do not infer
approval from silence. The user may respond or intervene directly in the child
buffer; refresh its state if a request is stale.
