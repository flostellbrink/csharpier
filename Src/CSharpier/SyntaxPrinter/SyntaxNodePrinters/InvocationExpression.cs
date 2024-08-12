namespace CSharpier.SyntaxPrinter.SyntaxNodePrinters;

internal record PrintedNode(CSharpSyntaxNode Node, Doc Doc);

internal static class InvocationExpression
{
    public static Doc Print(InvocationExpressionSyntax node, FormattingContext context)
    {
        return PrintMemberChain(node, context);
    }

    public static Doc PrintMemberChain(ExpressionSyntax node, FormattingContext context)
    {
        var parent = node.Parent;
        var printedNodes = new List<PrintedNode>();

        FlattenAndPrintNodes(node, printedNodes, context);

        return GroupPrintedNodes(printedNodes, parent);
    }

    private static void FlattenAndPrintNodes(
        ExpressionSyntax expression,
        List<PrintedNode> printedNodes,
        FormattingContext context
    )
    {
        /*
          We need to flatten things out because the AST has them this way
          InvocationExpression
          Expression                        ArgumentList
          this.DoSomething().DoSomething    ()
          
          MemberAccessExpression
          Expression            OperatorToken   Name
          this.DoSomething()    .               DoSomething
          
          InvocationExpression
          Expression            ArgumentList
          this.DoSomething      ()
          
          MemberAccessExpression
          Expression    OperatorToken   Name
          this          .               DoSomething
          
          And we want to work with them from Left to Right
        */
        if (expression is InvocationExpressionSyntax invocationExpressionSyntax)
        {
            FlattenAndPrintNodes(invocationExpressionSyntax.Expression, printedNodes, context);
            printedNodes.Add(
                new PrintedNode(
                    invocationExpressionSyntax,
                    ArgumentList.Print(invocationExpressionSyntax.ArgumentList, context)
                )
            );
        }
        else if (expression is ElementAccessExpressionSyntax elementAccessExpression)
        {
            FlattenAndPrintNodes(elementAccessExpression.Expression, printedNodes, context);
            printedNodes.Add(
                new PrintedNode(
                    elementAccessExpression,
                    Node.Print(elementAccessExpression.ArgumentList, context)
                )
            );
        }
        else if (expression is MemberAccessExpressionSyntax memberAccessExpressionSyntax)
        {
            FlattenAndPrintNodes(memberAccessExpressionSyntax.Expression, printedNodes, context);
            printedNodes.Add(
                new PrintedNode(
                    memberAccessExpressionSyntax,
                    Doc.Concat(
                        Token.Print(memberAccessExpressionSyntax.OperatorToken, context),
                        Node.Print(memberAccessExpressionSyntax.Name, context)
                    )
                )
            );
        }
        else if (expression is ConditionalAccessExpressionSyntax conditionalAccessExpressionSyntax)
        {
            FlattenAndPrintNodes(
                conditionalAccessExpressionSyntax.Expression,
                printedNodes,
                context
            );
            printedNodes.Add(
                new PrintedNode(
                    conditionalAccessExpressionSyntax,
                    Token.Print(conditionalAccessExpressionSyntax.OperatorToken, context)
                )
            );
            FlattenAndPrintNodes(
                conditionalAccessExpressionSyntax.WhenNotNull,
                printedNodes,
                context
            );
        }
        else if (expression is PostfixUnaryExpressionSyntax postfixUnaryExpression)
        {
            FlattenAndPrintNodes(postfixUnaryExpression.Operand, printedNodes, context);
            printedNodes.Add(
                new PrintedNode(
                    postfixUnaryExpression,
                    Token.Print(postfixUnaryExpression.OperatorToken, context)
                )
            );
        }
        else
        {
            printedNodes.Add(new PrintedNode(expression, Node.Print(expression, context)));
        }
    }

    private static Doc GroupPrintedNodes(List<PrintedNode> printedNodes, SyntaxNode? parent)
    {
        // We want to group the printed nodes in the following manner:
        // [longish[[?.b.c!.d].e()][[.f.g].h()]]

        // Short first nodes will be folded into the first inner/middle group
        // [[[[[a?.b].c!.d]].e()][[.f.g].h()]]

        // Member groups for consecutive MemberAccessExpression, unless they are part of an InvocationExpression
        // Invocation groups for MemberAccessExpression with InvocationExpression
        // Outer group for all nodes

        // This makes sure that MemberAccessExpression stay together as long as possible
        // Then InvocationExpression will stay with the closest MemberAccessExpression as long as possible
        // And finally if possible everything will stay together

        if (printedNodes.Count == 0)
        {
            return Doc.Null;
        }

        Doc oneLine = Doc.Concat(printedNodes.Select(node => node.Doc).ToArray());
        var isChain =
            printedNodes.Count(node => node.Node is InvocationExpressionSyntax) > 1
            || printedNodes.Count(node =>
                node.Node
                    is MemberAccessExpressionSyntax
                        or MemberBindingExpressionSyntax
                        or ElementAccessExpressionSyntax
            ) > 1;
        var isString =
            printedNodes[0].Node is InterpolatedStringExpressionSyntax
            || printedNodes[0].Node
                is LiteralExpressionSyntax { Token: { ValueText: string value } };
        if (!isChain || isString)
        {
            return oneLine;
        }

        var index = 0;

        bool IsBase(CSharpSyntaxNode node) =>
            node
                is IdentifierNameSyntax
                    or ThisExpressionSyntax
                    or PredefinedTypeSyntax
                    or BaseExpressionSyntax
                    or GenericNameSyntax
                    or ObjectCreationExpressionSyntax
                    or MemberAccessExpressionSyntax
                    or MemberBindingExpressionSyntax
                    or ParenthesizedExpressionSyntax
                    or LiteralExpressionSyntax
                    or InterpolatedStringExpressionSyntax
                    or ArrayCreationExpressionSyntax;

        // First node should not be indented
        var shouldSeparateFirstNode =
            IsBase(printedNodes[index].Node)
            || parent
                is SimpleLambdaExpressionSyntax
                    or ArgumentSyntax
                    or BinaryExpressionSyntax
                    or ExpressionStatementSyntax;

        Doc? firstNode = null;
        if (shouldSeparateFirstNode)
        {
            var invocation = PeekInvocation(index);
            var member = PeekMemberAccess(index);
            if (invocation != null)
            {
                firstNode = invocation.Value.invocation;
                index = invocation.Value.index;
            }
            else if (member != null)
            {
                firstNode = member.Value.member;
                index = member.Value.index;
            }
            else
            {
                throw new Exception($"Unexpected first node {printedNodes[index].Node.GetType()}");
            }
        }

        // If first node has a break, we cannot fold it
        var firstNodeHasBreak = firstNode != null && DocUtilities.ContainsBreak(firstNode);

        // Fold first node into the first group if it's short
        var shouldFoldFirstNode =
            shouldSeparateFirstNode
            && !firstNodeHasBreak
            && index == 1
            && printedNodes[0].Node
                is IdentifierNameSyntax { Identifier.Text.Length: <= 4 }
                    or ThisExpressionSyntax
                    or PredefinedTypeSyntax
                    or BaseExpressionSyntax;

        // Keeps track of whether the first node has been folded into the first group
        var foldFirstNodeIntoNextGroup = shouldFoldFirstNode;

        var outerNodes = ParseOuter();

        // FIXME: Indents should happen inside parse invocation/members

        Doc expanded = firstNode switch
        {
            null => Doc.Group(Doc.Concat(outerNodes)),
            _ => Doc.Group(firstNode!, Doc.Indent(outerNodes))
        };

        return firstNodeHasBreak switch
        {
            true => expanded,
            false => Doc.ConditionalGroup(oneLine, expanded)
        };

        List<Doc> ParseOuter()
        {
            var nodes = new List<Doc>();
            while (index < printedNodes.Count)
            {
                var middle = ParseInvocationsGroup();
                if (middle == null)
                {
                    throw new Exception($"Unexpected node {printedNodes[index].Node.GetType()}");
                }

                nodes.Add(middle);
                continue;
            }

            return nodes;
        }

        Doc? ParseInvocationsGroup()
        {
            var nodes = new List<Doc>();

            while (index < printedNodes.Count)
            {
                var invocation = PeekInvocation(index);
                if (invocation != null)
                {
                    if (foldFirstNodeIntoNextGroup)
                    {
                        firstNode = Doc.Group(firstNode!, invocation.Value.invocation);
                        foldFirstNodeIntoNextGroup = false;
                        nodes.Add(Doc.Null);
                    }
                    else
                    {
                        nodes.Add(firstNodeHasBreak || isChain ? Doc.HardLine : Doc.SoftLine);
                        nodes.Add(invocation.Value.invocation);
                    }
                    index = invocation.Value.index;
                    continue;
                }

                var inner = ParseMembersGroup();
                if (inner != null)
                {
                    nodes.Add(inner);
                    continue;
                }

                break;
            }

            if (nodes.Count == 0)
            {
                return null;
            }

            return Doc.Group(nodes);
        }

        Doc? ParseMembersGroup()
        {
            var nodes = new List<Doc>();

            while (index < printedNodes.Count)
            {
                var memberAccess = PeekMemberAccess(index);
                if (memberAccess != null)
                {
                    if (foldFirstNodeIntoNextGroup)
                    {
                        firstNode = Doc.Group(firstNode!, memberAccess.Value.member);
                        foldFirstNodeIntoNextGroup = false;
                        nodes.Add(Doc.Null);
                    }
                    else
                    {
                        nodes.Add(Doc.SoftLine);
                        nodes.Add(memberAccess.Value.member);
                    }
                    index = memberAccess.Value.index;

                    if (index < printedNodes.Count && PeekInvocation(index) == null)
                    {
                        continue;
                    }
                }

                break;
            }

            if (nodes.Count == 0)
            {
                return null;
            }

            return Doc.Group(nodes);
        }

        // Returns invocation (?.Method() or .Method() or ?.Method()! or .Method()!) if it exists
        // Also returns chained invocations when methods return methods, Method()()
        (Doc invocation, int index)? PeekInvocation(int tempIndex)
        {
            var nodes = new List<Doc>();

            var member = PeekMemberAccess(tempIndex);
            if (member != null)
            {
                nodes.Add(member.Value.member);
                tempIndex = member.Value.index;
            }

            var found = false;
            while (tempIndex < printedNodes.Count)
            {
                if (printedNodes[tempIndex].Node is InvocationExpressionSyntax)
                {
                    nodes.Add(printedNodes[tempIndex].Doc);
                    tempIndex++;
                    found = true;
                }
                else
                {
                    break;
                }

                var postfix = PeekPostfix(tempIndex);
                if (postfix != null)
                {
                    nodes.Add(postfix.Value.postfix);
                    tempIndex = postfix.Value.index;
                }
            }

            if (!found)
            {
                return null;
            }

            return (Doc.Group(nodes), tempIndex);
        }

        // Returns member access (?.Member or .Member or ?.Member! or .Member!) if it exists
        // Also returns trailing ElementAccessExpressionSyntax, ?.Member[0] or .Member[0]
        (Doc member, int index)? PeekMemberAccess(int tempIndex)
        {
            var nodes = new List<Doc>();

            var access = PeekAccess(tempIndex);
            if (access != null)
            {
                nodes.Add(access.Value.access);
                tempIndex = access.Value.index;
            }
            if (IsBase(printedNodes[tempIndex].Node))
            {
                nodes.Add(printedNodes[tempIndex].Doc);
                tempIndex++;
            }
            else
            {
                return null;
            }

            {
                var postfix = PeekPostfix(tempIndex);
                if (postfix != null)
                {
                    nodes.Add(postfix.Value.postfix);
                    tempIndex = postfix.Value.index;
                }
            }

            while (tempIndex < printedNodes.Count)
            {
                if (printedNodes[tempIndex].Node is ElementAccessExpressionSyntax)
                {
                    nodes.Add(printedNodes[tempIndex].Doc);
                    tempIndex++;
                }
                else
                {
                    break;
                }

                var postfix = PeekPostfix(tempIndex);
                if (postfix != null)
                {
                    nodes.Add(postfix.Value.postfix);
                    tempIndex = postfix.Value.index;
                }
            }

            return (Doc.Group(nodes), tempIndex);
        }

        // Returns leading conditional access (?.) if it exists
        (Doc access, int index)? PeekAccess(int tempIndex)
        {
            if (printedNodes[tempIndex].Node is ConditionalAccessExpressionSyntax)
            {
                return (printedNodes[tempIndex].Doc, tempIndex + 1);
            }

            return null;
        }

        // Returns trailing postfix (!) if it exists
        (Doc postfix, int index)? PeekPostfix(int tempIndex)
        {
            if (tempIndex >= printedNodes.Count)
            {
                return null;
            }

            if (printedNodes[tempIndex].Node is PostfixUnaryExpressionSyntax)
            {
                return (printedNodes[tempIndex].Doc, tempIndex + 1);
            }

            return null;
        }
    }
}
